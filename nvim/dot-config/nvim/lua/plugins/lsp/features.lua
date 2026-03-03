local M = {}

---@param title string
---@param items vim.quickfix.entry[]
---@param use_snacks? boolean
local function open_list(title, items, use_snacks)
  vim.fn.setqflist({}, " ", { title = title, items = items })
  if use_snacks then
    local ok, snacks = pcall(require, "snacks")
    if ok and snacks.picker and snacks.picker.qflist then
      snacks.picker.qflist()
      return
    end
  end
  vim.cmd("copen")
end

---@param item lsp.TypeHierarchyItem|lsp.CallHierarchyItem
---@return string
local function hierarchy_item_key(item)
  local start = (item.selectionRange and item.selectionRange.start) or { line = 0, character = 0 }
  return string.format("%s:%s:%d:%d", item.uri or "", item.name or "", start.line or 0, start.character or 0)
end

---@param nodes { item: lsp.TypeHierarchyItem|lsp.CallHierarchyItem, depth: integer }[]
---@param title string
---@param use_snacks? boolean
local function open_hierarchy_list(nodes, title, use_snacks)
  local items = {}
  for _, node in ipairs(nodes) do
    local item = node.item
    local kind = vim.lsp.protocol.SymbolKind[item.kind] or "Unknown"
    local prefix = string.rep("  ", node.depth) .. (node.depth == 0 and "* " or "- ")
    table.insert(items, {
      filename = vim.uri_to_fname(item.uri),
      lnum = item.selectionRange.start.line + 1,
      col = item.selectionRange.start.character + 1,
      text = string.format("%s%s (%s)", prefix, item.name, kind),
    })
  end
  open_list(title, items, use_snacks)
end

---@param bufnr integer
---@param capability string
---@param prepare_method string
---@return vim.lsp.Client?
local function get_hierarchy_client(bufnr, capability, prepare_method)
  for _, client in ipairs(vim.lsp.get_clients({ bufnr = bufnr })) do
    if client.server_capabilities and client.server_capabilities[capability] then
      return client
    end
    if client:supports_method(prepare_method) then
      return client
    end
  end
  return nil
end

---@param bufnr integer
---@param root_item lsp.TypeHierarchyItem|lsp.CallHierarchyItem
---@param fetch_children fun(item: lsp.TypeHierarchyItem|lsp.CallHierarchyItem, cb: fun(err: lsp.ResponseError?, children: (lsp.TypeHierarchyItem|lsp.CallHierarchyItem)[]?))
---@param done fun(err: lsp.ResponseError?, nodes: { item: lsp.TypeHierarchyItem|lsp.CallHierarchyItem, depth: integer }[]?)
local function collect_hierarchy(bufnr, root_item, fetch_children, done)
  local queue = { { item = root_item, depth = 0 } }
  local nodes = { { item = root_item, depth = 0 } }
  local seen = { [hierarchy_item_key(root_item)] = true }
  local cursor = 1
  local pending = 0
  local finished = false

  local function finalize(err)
    if finished then
      return
    end
    if err then
      finished = true
      done(err, nil)
      return
    end
    if pending == 0 and cursor > #queue then
      finished = true
      done(nil, nodes)
    end
  end

  local function pump()
    while cursor <= #queue do
      local current = queue[cursor]
      cursor = cursor + 1
      pending = pending + 1

      fetch_children(current.item, function(err, children)
        pending = pending - 1
        if err then
          finalize(err)
          return
        end

        if children then
          for _, child in ipairs(children) do
            local key = hierarchy_item_key(child)
            if not seen[key] then
              seen[key] = true
              local node = { item = child, depth = current.depth + 1 }
              table.insert(nodes, node)
              table.insert(queue, node)
            end
          end
        end

        if cursor <= #queue then
          pump()
        else
          finalize(nil)
        end
      end)
    end

    finalize(nil)
  end

  pump()
end

---@param direction 'supertypes'|'subtypes'
---@param bufnr? integer
---@param opts? { use_snacks?: boolean }
function M.show_type_hierarchy(direction, bufnr, opts)
  local target_bufnr = bufnr or 0
  local options = opts or {}
  local method = direction == "supertypes" and "typeHierarchy/supertypes" or "typeHierarchy/subtypes"
  local client = get_hierarchy_client(target_bufnr, "typeHierarchyProvider", "textDocument/prepareTypeHierarchy")

  if not client then
    vim.notify("LSP server does not support type hierarchy", vim.log.levels.WARN)
    return
  end

  local params = vim.lsp.util.make_position_params(0, client.offset_encoding)
  client:request("textDocument/prepareTypeHierarchy", params, function(err, result)
    if err then
      vim.notify(err.message or "Failed to prepare type hierarchy", vim.log.levels.ERROR)
      return
    end
    if not result or vim.tbl_isempty(result) then
      vim.notify("No type hierarchy available at cursor", vim.log.levels.INFO)
      return
    end

    collect_hierarchy(target_bufnr, result[1], function(item, cb)
      client:request(method, { item = item }, function(request_err, children)
        cb(request_err, children)
      end, target_bufnr)
    end, function(request_err, nodes)
      if request_err then
        vim.notify(request_err.message or "Failed to load type hierarchy", vim.log.levels.ERROR)
        return
      end
      if not nodes or #nodes <= 1 then
        local label = direction == "supertypes" and "No supertypes found" or "No subtypes found"
        vim.notify(label, vim.log.levels.INFO)
        return
      end
      local title = direction == "supertypes" and "Type Hierarchy: Supertypes" or "Type Hierarchy: Subtypes"
      open_hierarchy_list(nodes, title, options.use_snacks)
    end)
  end, target_bufnr)
end

---@param direction 'incoming'|'outgoing'
---@param bufnr? integer
---@param opts? { use_snacks?: boolean }
function M.show_call_hierarchy(direction, bufnr, opts)
  local target_bufnr = bufnr or 0
  local options = opts or {}
  local method = direction == "incoming" and "callHierarchy/incomingCalls" or "callHierarchy/outgoingCalls"
  local client = get_hierarchy_client(target_bufnr, "callHierarchyProvider", "textDocument/prepareCallHierarchy")

  if not client then
    vim.notify("LSP server does not support call hierarchy", vim.log.levels.WARN)
    return
  end

  local params = vim.lsp.util.make_position_params(0, client.offset_encoding)
  client:request("textDocument/prepareCallHierarchy", params, function(err, result)
    if err then
      vim.notify(err.message or "Failed to prepare call hierarchy", vim.log.levels.ERROR)
      return
    end
    if not result or vim.tbl_isempty(result) then
      vim.notify("No call hierarchy available at cursor", vim.log.levels.INFO)
      return
    end

    collect_hierarchy(target_bufnr, result[1], function(item, cb)
      client:request(method, { item = item }, function(request_err, relations)
        if request_err then
          cb(request_err, nil)
          return
        end
        local children = {}
        if relations then
          for _, relation in ipairs(relations) do
            table.insert(children, direction == "incoming" and relation.from or relation.to)
          end
        end
        cb(nil, children)
      end, target_bufnr)
    end, function(request_err, nodes)
      if request_err then
        vim.notify(request_err.message or "Failed to load call hierarchy", vim.log.levels.ERROR)
        return
      end
      if not nodes or #nodes <= 1 then
        local label = direction == "incoming" and "No incoming calls found" or "No outgoing calls found"
        vim.notify(label, vim.log.levels.INFO)
        return
      end
      local title = direction == "incoming" and "Call Hierarchy: Incoming" or "Call Hierarchy: Outgoing"
      open_hierarchy_list(nodes, title, options.use_snacks)
    end)
  end, target_bufnr)
end

---@param action table
---@return lsp.Command?
local function normalize_command(action)
  if type(action.command) == "table" then
    return action.command
  end
  if type(action.command) == "string" then
    return {
      title = action.title,
      command = action.command,
      arguments = action.arguments,
    }
  end
  return nil
end

---@param entry { action: table, client_id: integer }
---@param bufnr integer
---@param done fun(ok: boolean)?
function M.apply_code_action_entry(entry, bufnr, done)
  local client = vim.lsp.get_client_by_id(entry.client_id)
  local action = entry.action

  local function finish(ok)
    if done then
      done(ok)
    end
  end

  local function apply_action(resolved_action)
    local offset_encoding = (client and client.offset_encoding) or "utf-16"

    if resolved_action.edit then
      vim.lsp.util.apply_workspace_edit(resolved_action.edit, offset_encoding)
    end

    local command = normalize_command(resolved_action)
    if command then
      local executed = false
      if client then
        executed = pcall(client.exec_cmd, client, command, { bufnr = bufnr })
      end
      if not executed then
        pcall(vim.lsp.buf.execute_command, command)
      end
    end

    finish(true)
  end

  if client and not action.edit and not action.command and client:supports_method("codeAction/resolve") then
    client:request("codeAction/resolve", action, function(err, resolved_action)
      if err then
        vim.notify(err.message or "Failed to resolve code action", vim.log.levels.ERROR)
        finish(false)
        return
      end
      apply_action(resolved_action or action)
    end, bufnr)
    return
  end

  apply_action(action)
end

---@param bufnr integer
---@param done fun(entries: { action: table, client_id: integer }[])
local function collect_code_action_entries(bufnr, done)
  local clients = vim.lsp.get_clients({ bufnr = bufnr, method = "textDocument/codeAction" })
  if #clients == 0 then
    done({})
    return
  end

  local cursor = vim.api.nvim_win_get_cursor(0)
  local line = cursor[1] - 1
  local entries = {}
  local pending = #clients

  local function finish_client()
    pending = pending - 1
    if pending == 0 then
      done(entries)
    end
  end

  for _, client in ipairs(clients) do
    local params = vim.lsp.util.make_range_params(0, client.offset_encoding)
    params.context = {
      diagnostics = vim.diagnostic.get(bufnr, { lnum = line }),
    }

    client:request("textDocument/codeAction", params, function(err, actions)
      if not err and actions then
        for _, action in ipairs(actions) do
          table.insert(entries, { action = action, client_id = client.id })
        end
      end
      finish_client()
    end, bufnr)
  end
end

---@param kind string?
---@return string
local function code_action_category(kind)
  if not kind or kind == "" then
    return "other"
  end
  if kind:match("^source%.fixAll") then
    return "source.fixAll"
  end
  if kind:match("^source%.organizeImports") then
    return "source.organizeImports"
  end
  if kind:match("^source") then
    return "source"
  end
  return kind:match("^[^%.]+") or "other"
end

---@param bufnr? integer
function M.code_action_browser(bufnr)
  local target_bufnr = bufnr or 0
  collect_code_action_entries(target_bufnr, function(entries)
    if #entries == 0 then
      vim.notify("No code actions available", vim.log.levels.INFO)
      return
    end

    local grouped = {}
    for _, entry in ipairs(entries) do
      local category = code_action_category(entry.action.kind)
      grouped[category] = grouped[category] or {}
      table.insert(grouped[category], entry)
    end

    local categories = {}
    for name, grouped_entries in pairs(grouped) do
      table.insert(categories, {
        name = name,
        label = string.format("%s (%d)", name, #grouped_entries),
        entries = grouped_entries,
      })
    end
    table.sort(categories, function(a, b)
      return a.name < b.name
    end)

    vim.ui.select(categories, {
      prompt = "Code action category",
      format_item = function(item)
        return item.label
      end,
    }, function(category)
      if not category then
        return
      end

      table.sort(category.entries, function(a, b)
        return (a.action.title or "") < (b.action.title or "")
      end)

      vim.ui.select(category.entries, {
        prompt = "Code action",
        format_item = function(item)
          local kind = item.action.kind or "other"
          return string.format("[%s] %s", kind, item.action.title or "<untitled>")
        end,
      }, function(selected)
        if not selected then
          return
        end
        M.apply_code_action_entry(selected, target_bufnr)
      end)
    end)
  end)
end

---@param bufnr? integer
function M.apply_safe_source_fixes(bufnr)
  local target_bufnr = bufnr or 0
  collect_code_action_entries(target_bufnr, function(entries)
    local safe = {}
    for _, entry in ipairs(entries) do
      local kind = entry.action.kind or ""
      if kind:match("^source%.fixAll") or kind:match("^source%.organizeImports") then
        table.insert(safe, entry)
      end
    end

    if #safe == 0 then
      vim.notify("No safe source fixes available", vim.log.levels.INFO)
      return
    end

    table.sort(safe, function(a, b)
      local ak = a.action.kind or ""
      local bk = b.action.kind or ""
      if ak:match("^source%.fixAll") and not bk:match("^source%.fixAll") then
        return true
      end
      if bk:match("^source%.fixAll") and not ak:match("^source%.fixAll") then
        return false
      end
      return (a.action.title or "") < (b.action.title or "")
    end)

    local idx = 1
    local function apply_next()
      if idx > #safe then
        vim.notify(string.format("Applied %d safe source actions", #safe), vim.log.levels.INFO)
        return
      end
      local current = safe[idx]
      idx = idx + 1
      M.apply_code_action_entry(current, target_bufnr, function()
        apply_next()
      end)
    end

    apply_next()
  end)
end

---@param bufnr? integer
---@param opts? { use_snacks?: boolean }
---@param done? fun(ref_count: integer)
function M.preview_rename_impact(bufnr, opts, done)
  local target_bufnr = bufnr or 0
  local options = opts or {}
  local client = nil
  for _, c in ipairs(vim.lsp.get_clients({ bufnr = target_bufnr })) do
    if c:supports_method("textDocument/references") then
      client = c
      break
    end
  end

  if not client then
    vim.notify("LSP server does not support references", vim.log.levels.WARN)
    if done then
      done(0)
    end
    return
  end

  local params = vim.lsp.util.make_position_params(0, client.offset_encoding)
  params.context = { includeDeclaration = true }

  client:request("textDocument/references", params, function(err, locations)
    if err then
      vim.notify(err.message or "Failed to load references", vim.log.levels.ERROR)
      if done then
        done(0)
      end
      return
    end

    if not locations or vim.tbl_isempty(locations) then
      vim.notify("No references found", vim.log.levels.INFO)
      if done then
        done(0)
      end
      return
    end

    local qf_items = {}
    for _, location in ipairs(locations) do
      local uri = location.uri or location.targetUri
      local range = location.range or location.targetSelectionRange
      if uri and range and range.start then
        table.insert(qf_items, {
          filename = vim.uri_to_fname(uri),
          lnum = range.start.line + 1,
          col = range.start.character + 1,
          text = "rename impact",
        })
      end
    end

    open_list(string.format("Rename Impact (%d)", #qf_items), qf_items, options.use_snacks)
    if done then
      done(#qf_items)
    end
  end, target_bufnr)
end

---@param bufnr? integer
---@param opts? { use_snacks?: boolean }
function M.rename_with_preview(bufnr, opts)
  local target_bufnr = bufnr or 0
  local old_name = vim.fn.expand("<cword>")

  M.preview_rename_impact(target_bufnr, opts, function(ref_count)
    if ref_count == 0 then
      return
    end

    vim.ui.input({
      prompt = string.format("Rename '%s' to: ", old_name),
      default = old_name,
    }, function(new_name)
      if not new_name or new_name == "" or new_name == old_name then
        return
      end

      local ok = pcall(vim.lsp.buf.rename, new_name)
      if not ok then
        vim.lsp.buf.rename()
      end
      vim.notify(string.format("Rename requested (%d locations previewed)", ref_count), vim.log.levels.INFO)
    end)
  end)
end

return M

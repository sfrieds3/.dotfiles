local function scratch()
  vim.bo.buftype = "nofile"
  vim.bo.bufhidden = "wipe"
  vim.bo.swapfile = false
end

local function scratch_to_quickfix(close_qf)
  local items, bufnr = {}, vim.api.nvim_get_current_buf()
  for _, line in ipairs(vim.api.nvim_buf_get_lines(bufnr, 0, -1, false)) do
    if line ~= "" then
      local f, lnum, text = line:match("^([^:]+):(%d+):(.*)$")
      if f and lnum then
        table.insert(items, { filename = vim.fn.fnamemodify(f, ":p"), lnum = tonumber(lnum), text = text }) -- for grep filename:line:text
      else
        local lnum, text = line:match("^(%d+):(.*)$")
        if lnum and text then
          table.insert(items, { filename = vim.fn.bufname(vim.fn.bufnr("#")), lnum = tonumber(lnum), text = text }) -- for current buffer grep
        else
          table.insert(items, { filename = vim.fn.fnamemodify(line, ":p") }) -- for find results, only fnames
        end
      end
    end
  end
  vim.api.nvim_buf_delete(bufnr, { force = true })
  vim.fn.setqflist(items, "r")
  vim.cmd("copen | cc")
  if close_qf then
    vim.cmd("cclose")
  end
end
local function extcmd(cmd, qf, close_qf, novsplit)
  output = vim.fn.systemlist(cmd)
  if not output or #output == 0 then
    return
  end
  vim.cmd(novsplit and "enew" or "vnew")
  vim.api.nvim_buf_set_lines(0, 0, -1, false, output)
  scratch()
  if qf then
    scratch_to_quickfix(close_qf)
  end
end

vim.keymap.set("n", "<leader>Q", scratch_to_quickfix)
-- vim.keymap.set("n", "<leader>ss", function()
--   vim.ui.input({ prompt = "> " }, function(p)
--     if p then
--       extcmd("grep -in '" .. p .. "' " .. vim.fn.shellescape(vim.api.nvim_buf_get_name(0)), false)
--     end
--   end)
-- end)
-- vim.keymap.set("n", "<leader>sg", function()
--   vim.ui.input({ prompt = "> " }, function(p)
--     if p then
--       local path, excludes, ex = pre_search()
--       for _, pat in ipairs(excludes) do
--         table.insert(ex, string.format("--exclude-dir='%s'", pat))
--       end
--       extcmd(string.format("grep -IEnr %s '%s' %s", table.concat(ex, " "), p, path), true)
--     end
--   end)
-- end)
-- vim.keymap.set("n", "<leader>sf", function()
--   vim.ui.input({ prompt = "> " }, function(p)
--     if p then
--       local path, excludes, ex = pre_search()
--       for _, pat in ipairs(excludes) do
--         table.insert(ex, string.format("-path '*%s*' -prune -o", pat))
--       end
--       extcmd(
--         string.format("find %s %s -path '*%s*' -print", vim.fn.shellescape(path), table.concat(ex, " "), p),
--         true,
--         true
--       )
--     end
--   end)
-- end)
-- vim.keymap.set("n", "<leader>l", function()
--   local bn, ft = vim.fn.expand("%"), vim.bo.filetype
--   if ft == "python" then
--     extcmd("isort -q " .. bn .. "&& black -q " .. bn)
--     extcmd("ruff check --output-format=concise --quiet " .. bn, true)
--     vim.cmd("edit")
--   elseif ft == "rust" then
--     vim.fn.systemlist("cargo fmt")
--     extcmd("cargo check && cargo clippy")
--   end
-- end)
-- vim.keymap.set("n", "<leader>c", function()
--   vim.ui.input({ prompt = "> " }, function(c)
--     if c then
--       extcmd(c)
--     end
--   end)
-- end)

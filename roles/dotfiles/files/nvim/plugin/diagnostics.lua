local group = require("utils.utils").augroup("diagnostic-highlights")
vim.api.nvim_create_autocmd("ColorScheme", {
  group = group,
  callback = function()
    vim.diagnostic.config({
      -- virtual_text = { source = false },
      float = {
        source = true,
        border = "rounded",
        title = "Diagnostics",
        title_pos = "left",
        header = "",
      },
      underline = true,
      update_in_insert = false,
      severity_sort = true,
      virtual_text = false,
      signs = {
        text = {
          [vim.diagnostic.severity.ERROR] = "",
          [vim.diagnostic.severity.WARN] = "",
          [vim.diagnostic.severity.INFO] = "",
          [vim.diagnostic.severity.HINT] = "",
        },
        texthl = {
          [vim.diagnostic.severity.ERROR] = "DiagnosticSignError",
          [vim.diagnostic.severity.WARN] = "DiagnosticSignWarn",
          [vim.diagnostic.severity.INFO] = "DiagnosticSignInfo",
          [vim.diagnostic.severity.HINT] = "DiagnosticSignHint",
        },
      },
    })
  end,
})

local virtual_text_config = {
  format = function(diagnostic)
    -- Replace newline and tab characters with space for more compact diagnostics
    local message = diagnostic.message:gsub("\n", " "):gsub("\t", " "):gsub("%s+", " "):gsub("^%s+", "")
    return message
  end,
}

vim.api.nvim_create_user_command("EnableVirtualText", function()
  vim.diagnostic.config({ virtual_text = virtual_text_config })
  print("virtual_text enabled")
end, {})

vim.api.nvim_create_user_command("DisableVirtualText", function()
  vim.diagnostic.config({ virtual_text = false })
  print("virtual_text disabled")
end, {})

local virtual_text_enabled = false
vim.api.nvim_create_user_command("ToggleVirtualText", function()
  if virtual_text_enabled then
    vim.cmd.DisableVirtualText()
    virtual_text_enabled = false
  else
    vim.cmd.EnableVirtualText()
    virtual_text_enabled = true
  end
end, {})

local function goto_diagnostic(direction, severity, float)
  local goto_diag = direction == "previous" and vim.diagnostic.goto_prev or vim.diagnostic.goto_next
  severity = severity and vim.diagnostic.severity[severity] or nil
  float = float or false
  return function()
    goto_diag({ severity = severity, float = float })
  end
end

local function diagnostic_toggle_buf(bufnr)
  bufnr = bufnr or vim.api.nvim_get_current_buf()
  if vim.diagnostic.is_disabled(bufnr) then
    vim.diagnostic.enable(bufnr)
    print("Enabled buffer diagnostics")
  else
    vim.diagnostic.disable(bufnr)
    print("Disabled buffer diagnostics")
  end
end

-- See `:help vim.diagnostic.*` for documentation on any of the below functions
vim.keymap.set("n", "<leader>e", vim.diagnostic.open_float, { desc = "Line Diagnostics" })
vim.keymap.set("n", "<leader>cf", vim.diagnostic.open_float, { desc = "Line Diagnostics" })
vim.keymap.set("n", "[d", vim.diagnostic.goto_prev, { desc = "Go To Previous Diagnostic" })
vim.keymap.set("n", "]d", vim.diagnostic.goto_next, { desc = "Go To Next Diagnostic" })
vim.keymap.set("n", "<leader>q", vim.diagnostic.setloclist, { desc = "Diagnotics set Loclist" })
vim.keymap.set("n", "<leader>cD", vim.diagnostic.disable, { desc = "Disable Global Diagnostics" })
vim.keymap.set("n", "<leader>cd", function()
  vim.diagnostic.disable(vim.api.nvim_get_current_buf())
end, { desc = "Disable Buffer Diagnostics" })
vim.keymap.set("n", "<leader>cE", vim.diagnostic.enable, { desc = "Enable Global Diagnosics" })
vim.keymap.set("n", "<leader>ce", function()
  vim.diagnostic.enable(vim.api.nvim_get_current_buf())
end, { desc = "Enable Global Diagnosics" })
vim.keymap.set("n", "<leader>ct", diagnostic_toggle_buf, { desc = "Toggle Diagnostics in Buffer" })
vim.keymap.set("n", "]e", goto_diagnostic("next", "ERROR"), { desc = "Next Error" })
vim.keymap.set("n", "[e", goto_diagnostic("previous", "ERROR"), { desc = "Prev Error" })
vim.keymap.set("n", "]w", goto_diagnostic("next", "WARN"), { desc = "Next Warning" })
vim.keymap.set("n", "[w", goto_diagnostic("previous", "WARN"), { desc = "Prev Warning" })
vim.keymap.set("n", "<leader>ce", "<cmd>ToggleVirtualText<cr>", { desc = "Toggle Virtual Text" })

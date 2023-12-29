vim.fn.sign_define("DiagnosticSignError", { text = "", texthl = "DiagnosticSignError" })
vim.fn.sign_define("DiagnosticSignWarn", { text = "", texthl = "DiagnosticSignWarn" })
vim.fn.sign_define("DiagnosticSignInfo", { text = "", texthl = "DiagnosticSignInfo" })
vim.fn.sign_define("DiagnosticSignHint", { text = "", texthl = "DiagnosticSignHint" })

vim.diagnostic.config({
  -- virtual_text = { source = false },
  float = {
    source = "always",
    border = "rounded",
    title = "Diagnostics",
    title_pos = "left",
    header = "",
  },
  underline = true,
  signs = true,
  update_in_insert = false,
  severity_sort = true,
  virtual_text = false,
  -- virtual_text = {
  --   format = function(diagnostic)
  --     -- Replace newline and tab characters with space for more compact diagnostics
  --     local message = diagnostic.message:gsub("\n", " "):gsub("\t", " "):gsub("%s+", " "):gsub("^%s+", "")
  --     return message
  --   end,
  -- },
})

local function goto_diagnostic(severity, next)
  local goto_diag = next and vim.diagnostic.goto_next or vim.diagnostic.goto_prev
  severity = severity and vim.diagnostic.severity[severity] or nil
  return function()
    goto_diag({ severity = severity })
  end
end

-- See `:help vim.diagnostic.*` for documentation on any of the below functions
vim.keymap.set("n", "<leader>e", vim.diagnostic.open_float, { desc = "Line Diagnostics" })
vim.keymap.set("n", "<leader>cd", vim.diagnostic.open_float, { desc = "Line Diagnostics" })
vim.keymap.set("n", "[d", vim.diagnostic.goto_prev, { desc = "Go To Previous Diagnostic" })
vim.keymap.set("n", "]d", vim.diagnostic.goto_next, { desc = "Go To Next Diagnostic" })
vim.keymap.set("n", "<leader>q", vim.diagnostic.setloclist, { desc = "Diagnotics set Loclist" })
vim.keymap.set("n", "_DD", vim.diagnostic.disable, { desc = "Disable Diagnostics" })
vim.keymap.set("n", "_DE", vim.diagnostic.enable, { desc = "Enable Diagnosics" })
vim.keymap.set("n", "]e", goto_diagnostic(true, "ERROR"), { desc = "Next Error" })
vim.keymap.set("n", "[e", goto_diagnostic(false, "ERROR"), { desc = "Prev Error" })
vim.keymap.set("n", "]w", goto_diagnostic(true, "WARN"), { desc = "Next Warning" })
vim.keymap.set("n", "[w", goto_diagnostic(false, "WARN"), { desc = "Prev Warning" })

require("dapui").setup({})

require("dap").listeners.after.event_initialized["dapui_config"] = function()
  require("dapui").open({})
end

vim.keymap.set("n", "\\dd", require("dapui").toggle, { desc = "[d]ap-ui: toggle" })

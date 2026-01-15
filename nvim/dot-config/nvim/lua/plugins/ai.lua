return {
  "Exafunction/windsurf.nvim",
  dependencies = {
    "nvim-lua/plenary.nvim",
    "saghen/blink.cmp",
  },
  name = "codeium",
  opts = {
    enable_cmp_source = false,
  },
  init = function()
    vim.api.nvim_create_autocmd("VimLeavePre", {
      callback = function()
        local ok, codeium = pcall(require, "codeium")
        if ok and codeium.get_server then
          local server = codeium.get_server()
          if server then
            server:shutdown()
          end
        end
      end,
    })
  end,
}

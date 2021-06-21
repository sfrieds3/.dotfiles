local lsp_config = require("scwfri.pconfig.lsp_config")

if vim.fn.executable('lua-language-server')
  then
  require('nlua.lsp.nvim').setup(require('lspconfig'), {
      on_attach = lsp_config.on_attach
    })
end

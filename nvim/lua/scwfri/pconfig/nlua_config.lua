local lsp_config = require("scwfri.pconfig.lsp_config")

local cache_dir = vim.fn.stdpath('cache')
local bin_folder = 'Linux'
local bin_location = string.format(
  "%s/nlua/sumneko_lua/lua-language-server/bin/%s/lua-language-server",
  cache_dir,
  bin_folder
)

if (vim.fn.executable(bin_location) == 1) then
  require('nlua.lsp.nvim').setup(require('lspconfig'), {
      on_attach = lsp_config.on_attach
  })
end

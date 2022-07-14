vim.diagnostic.config({
 virtual_text = { source = true },
})

-- See `:help vim.diagnostic.*` for documentation on any of the below functions
local opts = { noremap=true, silent=true, buffer = bufnr }
vim.keymap.set('n', '<Space>e', vim.diagnostic.open_float, opts)
vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, opts)
vim.keymap.set('n', ']d', vim.diagnostic.goto_next, opts)
vim.keymap.set('n', '_q', vim.diagnostic.setloclist, opts)
vim.keymap.set('n', '_Dd', vim.diagnostic.disable, opts)
vim.keymap.set('n', '_DD', vim.diagnostic.disable, opts)
vim.keymap.set('n', '_De', vim.diagnostic.enable, opts)
vim.keymap.set('n', '_DE', vim.diagnostic.enable, opts)

local on_attach = function(client, bufnr)
  -- Enable completion triggered by <c-x><c-o>
  vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')

  local bufopts = { noremap = true, silent = true, buffer = bufnr}
  -- See `:help vim.lsp.*` for documentation on any of the below functions
  vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, bufopts)
  vim.keymap.set('n', 'gd', vim.lsp.buf.definition, bufopts)
  vim.keymap.set('n', 'K', vim.lsp.buf.hover, bufopts)
  vim.keymap.set('n', '_gi', vim.lsp.buf.implementation, bufopts)
  vim.keymap.set('n', '<C-k>', vim.lsp.buf.signature_help, bufopts)
  vim.keymap.set('n', '<Space>wa', vim.lsp.buf.add_workspace_folder, bufopts)
  vim.keymap.set('n', '<Space>wr', vim.lsp.buf.remove_workspace_folder, bufopts)
  vim.keymap.set('n', '<Space>wl', function()
    print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
  end, bufopts)
  vim.keymap.set('n', '<Space>D', vim.lsp.buf.type_definition, bufopts)
  vim.keymap.set('n', '<Space>rn', vim.lsp.buf.rename, bufopts)
  vim.keymap.set('n', '<Space>ca', vim.lsp.buf.code_action, bufopts)
  vim.keymap.set('n', '<Space>rf', vim.lsp.buf.references, bufopts)
  vim.keymap.set('n', '<Space>F', vim.lsp.buf.formatting, bufopts)

  require('aerial').on_attach(client, bufnr)
  require('illuminate').on_attach(client)
end

local capabilities = require('cmp_nvim_lsp').update_capabilities(vim.lsp.protocol.make_client_capabilities())
local lsp_flags = {
  debounce_text_changes = 150

}

if vim.fn.executable('pylsp') then
  require('lspconfig')['pylsp'].setup({
    on_attach = on_attach,
    flags = lsp_flags,
    capabilities = capabilities,
  })
end

if vim.fn.executable('clangd') then
  require('lspconfig')['clangd'].setup({
    on_attach = on_attach,
    flags = lsp_flags,
    capabilities = capabilities,
  })
end

if vim.fn.executable('rust_analyzer') then
  require('lspconfig')['rust_analyzer'].setup({
    on_attach = on_attach,
    flags = lsp_flags,
    capabilities = capabilities,
  })
end

if vim.fn.executable('tsserver') then
  require('lspconfig')['tsserver'].setup({
    on_attach = on_attach,
    flags = lsp_flags,
    capabilities = capabilities,
  })
end

if vim.fn.executable('vimls') then
  require('lspconfig')['vimls'].setup({
    on_attach = on_attach,
    flags = lsp_flags,
    capabilities = capabilities,
  })
end

-- sumneko
if vim.fn.executable('lua-language-server') == 1 then
  local runtime_path = vim.split(package.path, ';')
  table.insert(runtime_path, 'lua/?.lua')
  table.insert(runtime_path, 'lua/?/init.lua')

  require('lspconfig').sumneko_lua.setup {
    settings = {
      Lua = {
        runtime = {
          -- Tell the language server which version of Lua you're using (most likely LuaJIT in the case of Neovim)
          version = 'LuaJIT',
          -- Setup your lua path
          path = runtime_path,
        },
        diagnostics = {
          -- Get the language server to recognize the `vim` global
          globals = {'vim'},
        },
        workspace = {
          -- Make the server aware of Neovim runtime files
          library = vim.api.nvim_get_runtime_file('', true),
        },
        -- Do not send telemetry data containing a randomized but unique identifier
        telemetry = {
          enable = false,
        },
      },
    },
  }
end

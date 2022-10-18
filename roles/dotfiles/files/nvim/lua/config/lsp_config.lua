vim.diagnostic.config({
  virtual_text = { source = true },
  float = { source = true },
})

-- See `:help vim.diagnostic.*` for documentation on any of the below functions
local opts = { noremap = true, silent = true, buffer = bufnr }
vim.keymap.set("n", "<Space>e", vim.diagnostic.open_float, opts)
vim.keymap.set("n", "[d", vim.diagnostic.goto_prev, opts)
vim.keymap.set("n", "]d", vim.diagnostic.goto_next, opts)
vim.keymap.set("n", "_q", vim.diagnostic.setloclist, opts)
vim.keymap.set("n", "_Dd", vim.diagnostic.disable, opts)
vim.keymap.set("n", "_DD", vim.diagnostic.disable, opts)
vim.keymap.set("n", "_De", vim.diagnostic.enable, opts)
vim.keymap.set("n", "_DE", vim.diagnostic.enable, opts)

local on_attach = function(client, bufnr)
  -- Enable completion triggered by <c-x><c-o>
  vim.api.nvim_buf_set_option(bufnr, "omnifunc", "v:lua.vim.lsp.omnifunc")

  local bufopts = { noremap = true, silent = true, buffer = bufnr }
  -- See `:help vim.lsp.*` for documentation on any of the below functions
  vim.keymap.set("n", "gD", vim.lsp.buf.declaration, bufopts)
  vim.keymap.set("n", "gd", vim.lsp.buf.definition, bufopts)
  vim.keymap.set("n", "gI", vim.lsp.buf.implementation, bufopts)
  vim.keymap.set("n", "gT", vim.lsp.buf.type_definition, bufopts)
  vim.keymap.set("n", "K", vim.lsp.buf.hover, bufopts)
  vim.keymap.set("n", "<C-k>", vim.lsp.buf.signature_help, bufopts)
  vim.keymap.set("n", "<Space>wa", vim.lsp.buf.add_workspace_folder, bufopts)
  vim.keymap.set("n", "<Space>wr", vim.lsp.buf.remove_workspace_folder, bufopts)
  vim.keymap.set("n", "<Space>wl", function()
    print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
  end, bufopts)
  vim.keymap.set("n", "<Space>R", vim.lsp.buf.rename, bufopts)
  vim.keymap.set("n", "<Space>a", vim.lsp.buf.code_action, bufopts)
  vim.keymap.set("n", "<Space>r", vim.lsp.buf.references, bufopts)
  vim.keymap.set("n", "<Space>F", vim.lsp.buf.format, bufopts)

  require("aerial").on_attach(client, bufnr)
  require("illuminate").on_attach(client)
end

local capabilities = require("cmp_nvim_lsp").default_capabilities(vim.lsp.protocol.make_client_capabilities())

capabilities.textDocument.completion.completionItem.snippetSupport = true
capabilities.textDocument.completion.completionItem.preselectSupport = true
capabilities.textDocument.completion.completionItem.insertReplaceSupport = true
capabilities.textDocument.completion.completionItem.labelDetailsSupport = true
capabilities.textDocument.completion.completionItem.deprecatedSupport = true
capabilities.textDocument.completion.completionItem.commitCharactersSupport = true
capabilities.textDocument.completion.completionItem.tagSupport = { valueSet = { 1 } }
capabilities.textDocument.completion.completionItem.resolveSupport = {
  properties = {
    "documentation",
    "detail",
    "additionalTextEdits",
  },
}

local lsp_flags = {
  debounce_text_changes = 150,
}

local using_pyright = 0
local using_pylsp = 0
if vim.fn.executable("pyright") == 1 then
  using_pyright = 1
  require("lspconfig")["pyright"].setup({
    on_attach = on_attach,
    flags = lsp_flags,
    capabilities = capabilities,
  })
end

if vim.fn.executable("pylsp") == 1 and using_pyright == 0 then
  using_pylsp = 1
  require("lspconfig")["pylsp"].setup({
    on_attach = on_attach,
    flags = lsp_flags,
    capabilities = capabilities,
  })
end

if vim.fn.executable("gopls") == 1 and using_pylsp == 0 then
  require("lspconfig")["gopls"].setup({
    on_attach = on_attach,
    flags = lsp_flags,
    capabilities = capabilities,
    cmd = { "gopls", "serve" },
    filetypes = { "go", "gomod" },
    root_dir = require("lspconfig.util").root_pattern("go.work", "go.mod", ".git"),
    settings = {
      gopls = {
        analyses = {
          unusedparams = true,
        },
        staticcheck = true,
        linksInHover = true,
        usePlaceholders = true,
        codelenses = {
          generate = true,
          gc_details = true,
          regenerate_cgo = true,
          tidy = true,
          upgrade_dependency = true,
          vendor = true,
        },
      },
    },
  })

  function OrgImports(wait_ms)
    local params = vim.lsp.util.make_range_params()
    params.context = { only = { "source.organizeImports" } }
    local result = vim.lsp.buf_request_sync(0, "textDocument/codeAction", params, wait_ms)
    for _, res in pairs(result or {}) do
      for _, r in pairs(res.result or {}) do
        if r.edit then
          vim.lsp.util.apply_workspace_edit(r.edit, "UTF-8")
        else
          vim.lsp.buf.execute_command(r.command)
        end
      end
    end
  end

  vim.api.nvim_create_augroup("Golang", { clear = true })
  vim.api.nvim_create_autocmd("BufWritePre", {
    group = "Golang",
    pattern = "*.go",
    callback = function()
      vim.lsp.buf.format(nil, 1000)
      OrgImports(1000)
    end,
  })
end

if vim.fn.executable("clangd") == 1 then
  require("lspconfig")["clangd"].setup({
    on_attach = on_attach,
    flags = lsp_flags,
    capabilities = capabilities,
  })
end

if vim.fn.executable("rust_analyzer") == 1 then
  require("lspconfig")["rust_analyzer"].setup({
    on_attach = on_attach,
    flags = lsp_flags,
    capabilities = capabilities,
  })
end

if vim.fn.executable("tsserver") == 1 then
  require("lspconfig")["tsserver"].setup({
    on_attach = on_attach,
    flags = lsp_flags,
    capabilities = capabilities,
  })
end

if vim.fn.executable("vimls") == 1 then
  require("lspconfig")["vimls"].setup({
    on_attach = on_attach,
    flags = lsp_flags,
    capabilities = capabilities,
  })
end

if vim.fn.executable("ansiblels") == 1 then
  require("lspconfig")["ansiblels"].setup({
    on_attach = on_attach,
    flags = lsp_flags,
    capabilities = capabilities,
  })
end

-- sumneko
if vim.fn.executable("lua-language-server") == 1 then
  local runtime_path = vim.split(package.path, ";")
  table.insert(runtime_path, "lua/?.lua")
  table.insert(runtime_path, "lua/?/init.lua")

  require("lspconfig").sumneko_lua.setup({
    settings = {
      Lua = {
        runtime = {
          -- Tell the language server which version of Lua you're using (most likely LuaJIT in the case of Neovim)
          version = "LuaJIT",
          -- Setup your lua path
          path = runtime_path,
        },
        diagnostics = {
          -- Get the language server to recognize the `vim` global
          globals = { "vim" },
        },
        workspace = {
          -- Make the server aware of Neovim runtime files
          library = vim.api.nvim_get_runtime_file("", true),
        },
        -- Do not send telemetry data containing a randomized but unique identifier
        telemetry = {
          enable = false,
        },
      },
    },
  })
end

vim.diagnostic.config({
  virtual_text = { source = false },
  float = { source = true },
})

-- set up null-ls
require("null-ls").setup({
  sources = {
    -- code actions
    require("null-ls").builtins.code_actions.shellcheck,
    require("null-ls").builtins.code_actions.gitsigns,

    -- diagnostics
    require("null-ls").builtins.diagnostics.flake8,
    require("null-ls").builtins.diagnostics.trail_space.with({
      filetypes = { git = false },
    }),
    require("null-ls").builtins.diagnostics.shellcheck,

    -- formatting
    require("null-ls").builtins.formatting.stylua,
    require("null-ls").builtins.formatting.eslint,
    require("null-ls").builtins.formatting.yapf,
    require("null-ls").builtins.formatting.isort,
    require("null-ls").builtins.formatting.json_tool,
    require("null-ls").builtins.formatting.xmllint,
  },
})

-- See `:help vim.diagnostic.*` for documentation on any of the below functions
local opts = { noremap = true, silent = true }
vim.keymap.set("n", "<Space>e", vim.diagnostic.open_float, opts)
vim.keymap.set("n", "[d", vim.diagnostic.goto_prev, opts)
vim.keymap.set("n", "]d", vim.diagnostic.goto_next, opts)
vim.keymap.set("n", "_q", vim.diagnostic.setloclist, opts)
vim.keymap.set("n", "_DD", vim.diagnostic.disable, opts)
vim.keymap.set("n", "_DE", vim.diagnostic.enable, opts)
vim.keymap.set("n", "<space>q", vim.diagnostic.setloclist, opts)

local on_attach = function(client, bufnr)
  -- Enable completion triggered by <c-x><c-o>
  vim.api.nvim_buf_set_option(bufnr, "omnifunc", "v:lua.vim.lsp.omnifunc")

  local bufopts = { noremap = true, silent = true, buffer = bufnr }
  -- See `:help vim.lsp.*` for documentation on any of the below functions
  vim.keymap.set("n", "<Leader>gD", vim.lsp.buf.declaration, { buffer = bufnr, desc = "LSP: [g]oto [D]eclaration" })
  vim.keymap.set("n", "gd", vim.lsp.buf.definition, { buffer = bufnr, desc = "LSP: [g]oto [d]efinition" })
  vim.keymap.set("n", "gi", vim.lsp.buf.implementation, { buffer = bufnr, desc = "LSP: [g]oto [i]mplementation" })
  vim.keymap.set("n", "K", vim.lsp.buf.hover, { buffer = bufnr, desc = "LSP: hover" })
  vim.keymap.set("n", "<C-k>", vim.lsp.buf.signature_help, { buffer = bufnr, desc = "LSP: signatuer help" })
  vim.keymap.set(
    "n",
    "<Space>wa",
    vim.lsp.buf.add_workspace_folder,
    { buffer = bufnr, desc = "LSP: [w]orkspace [a]dd folder" }
  )
  vim.keymap.set(
    "n",
    "<Space>wr",
    vim.lsp.buf.remove_workspace_folder,
    { buffer = bufnr, desc = "LSP: [w]orkspace [r]emove folder" }
  )
  vim.keymap.set("n", "<Space>wl", function()
    print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
  end, { buffer = bufnr, desc = "LSP: [w]orkspace [l]ist folders" })
  vim.keymap.set("n", "<Space>D", vim.lsp.buf.type_definition, { buffer = bufnr, desc = "LSP: type [D]efinition" })
  vim.keymap.set("n", "<Space>R", vim.lsp.buf.rename, { buffer = bufnr, desc = "LSP: [R]ename" })
  vim.keymap.set("n", "<Space>A", vim.lsp.buf.code_action, { buffer = bufnr, desc = "LSP: code [A]ction" })
  vim.keymap.set("n", "gr", vim.lsp.buf.references, { buffer = bufnr, desc = "LSP: [g]oto [r]eferences" })
  vim.keymap.set("n", "<space>F", function()
    vim.lsp.buf.format({ async = true })
  end, { buffer = bufnr, desc = "LSP: async [F]ormat" })
  vim.keymap.set("x", "gq", vim.lsp.buf.format, { desc = "LSP: [f]ormat" })

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

if vim.fn.executable("typescript-language-server") == 1 then
  require("typescript").setup({
    server = {
      go_to_source_definition = {
        fallback = true,
      },
      on_attach = on_attach,
      flags = lsp_flags,
      capabilities = capabilities,
    },
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
  local runtime_path = vim.split(package.path, ";", {})
  table.insert(runtime_path, "lua/?.lua")
  table.insert(runtime_path, "lua/?/init.lua")

  require("lspconfig").sumneko_lua.setup({
    on_attach = on_attach,
    flags = lsp_flags,
    capabilities = capabilities,
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
          checkThirdParty = false,
        },
        -- Do not send telemetry data containing a randomized but unique identifier
        telemetry = {
          enable = false,
        },
      },
    },
  })
end

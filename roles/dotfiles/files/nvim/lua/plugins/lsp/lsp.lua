local M = {}

function M.setup()
  vim.diagnostic.config({
    virtual_text = { source = false },
    float = { source = true },
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
    vim.keymap.set("n", "<C-k>", vim.lsp.buf.signature_help, { buffer = bufnr, desc = "LSP: signature help" })
    vim.keymap.set("i", "<C-s>", vim.lsp.buf.signature_help, { buffer = bufnr, desc = "LSP: signature help" })
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

    if client.server_capabilities.documentSymbolProvider then
      require("nvim-navic").attach(client, bufnr)
    end
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

  require("lspconfig")["jsonls"].setup({
    on_attach = on_attach,
    flags = lsp_flags,
    capabilities = capabilities,
  })

  require("lspconfig")["dockerls"].setup({
    on_attach = on_attach,
    flags = lsp_flags,
    capabilities = capabilities,
  })

  require("lspconfig")["yamlls"].setup({
    on_attach = on_attach,
    flags = lsp_flags,
    capabilities = capabilities,
    settings = {
      yaml = {
        keyOrdering = false,
      },
    },
  })

  require("lspconfig")["jdtls"].setup({
    on_attach = on_attach,
    flags = lsp_flags,
    capabilities = capabilities,
  })

  require("lspconfig")["pyright"].setup({
    on_attach = on_attach,
    flags = lsp_flags,
    capabilities = capabilities,
  })

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

  require("lspconfig")["clangd"].setup({
    on_attach = on_attach,
    flags = lsp_flags,
    capabilities = capabilities,
  })

  require("lspconfig")["rust_analyzer"].setup({
    on_attach = on_attach,
    flags = lsp_flags,
    capabilities = capabilities,
  })

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

  require("lspconfig")["vimls"].setup({
    on_attach = on_attach,
    flags = lsp_flags,
    capabilities = capabilities,
  })

  require("lspconfig")["ansiblels"].setup({
    on_attach = on_attach,
    flags = lsp_flags,
    capabilities = capabilities,
  })

  local runtime_path = vim.split(package.path, ";", {})
  table.insert(runtime_path, "lua/?.lua")
  table.insert(runtime_path, "lua/?/init.lua")

  require("lspconfig").lua_ls.setup({
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

return M

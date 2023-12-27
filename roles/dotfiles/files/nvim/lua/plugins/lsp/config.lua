local M = {}
local lspconfig = require("lspconfig")

function M.setup()
  local function on_attach(client, bufnr)
    if client.server_capabilities.documentSymbolProvider then
      require("nvim-navic").attach(client, bufnr)
    end
  end

  local capabilities = require("cmp_nvim_lsp").default_capabilities(vim.lsp.protocol.make_client_capabilities())
  -- capabilities.textDocument.codeLens = true
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

  local config = { on_attach = on_attach, capabilities = capabilities }
  lspconfig.pyright.setup({ config })
  lspconfig.ruff_lsp.setup({ config })
  lspconfig.ast_grep.setup({ config })
  lspconfig.jsonls.setup({ config })
  lspconfig.dockerls.setup({ config })
  lspconfig.jdtls.setup({ config })
  lspconfig.clangd.setup({ config })
  lspconfig.rust_analyzer.setup({ config })
  lspconfig.tsserver.setup({ config })
  lspconfig.vimls.setup({ config })
  lspconfig.ansiblels.setup({ config })
  lspconfig.helm_ls.setup({ config })

  lspconfig.yamlls.setup({
    on_attach = on_attach,
    capabilities = capabilities,
    settings = {
      yaml = {
        keyOrdering = false,
      },
    },
  })

  lspconfig.gopls.setup({
    on_attach = on_attach,
    capabilities = capabilities,
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

  local runtime_path = vim.split(package.path, ";", {})
  table.insert(runtime_path, "lua/?.lua")
  table.insert(runtime_path, "lua/?/init.lua")

  lspconfig.lua_ls.setup({
    on_attach = on_attach,
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

  -- See `:help vim.diagnostic.*` for documentation on any of the below functions
  local opts = { noremap = true, silent = true }
  vim.keymap.set("n", "<Space>e", vim.diagnostic.open_float, opts)
  vim.keymap.set("n", "[d", vim.diagnostic.goto_prev, opts)
  vim.keymap.set("n", "]d", vim.diagnostic.goto_next, opts)
  vim.keymap.set("n", "_q", vim.diagnostic.setloclist, opts)
  vim.keymap.set("n", "_DD", vim.diagnostic.disable, opts)
  vim.keymap.set("n", "_DE", vim.diagnostic.enable, opts)
  vim.keymap.set("n", "<space>q", vim.diagnostic.setloclist, opts)

  vim.api.nvim_create_autocmd("LspAttach", {
    group = vim.api.nvim_create_augroup("UserLspConfig", {}),
    callback = function(ev)
      -- Enable completion triggered by <c-x><c-o>
      vim.bo[ev.buf].omnifunc = "v:lua.vim.lsp.omnifunc"

      -- Buffer local mappings.
      -- See `:help vim.lsp.*` for documentation on any of the below functions
      vim.keymap.set(
        "n",
        "<Leader>gD",
        vim.lsp.buf.declaration,
        { buffer = ev.buf, desc = "LSP: [g]oto [D]eclaration" }
      )
      vim.keymap.set("n", "gd", vim.lsp.buf.definition, { buffer = ev.buf, desc = "LSP: [g]oto [d]efinition" })
      vim.keymap.set("n", "gi", vim.lsp.buf.implementation, { buffer = ev.buf, desc = "LSP: [g]oto [i]mplementation" })
      vim.keymap.set("n", "K", vim.lsp.buf.hover, { buffer = ev.buf, desc = "LSP: hover" })
      vim.keymap.set("n", "<C-k>", vim.lsp.buf.signature_help, { buffer = ev.buf, desc = "LSP: signature help" })
      vim.keymap.set("i", "<C-s>", vim.lsp.buf.signature_help, { buffer = ev.buf, desc = "LSP: signature help" })
      vim.keymap.set(
        "n",
        "<Space>lwa",
        vim.lsp.buf.add_workspace_folder,
        { buffer = ev.buf, desc = "[L]SP: [w]orkspace [a]dd folder" }
      )
      vim.keymap.set(
        "n",
        "<Space>lwr",
        vim.lsp.buf.remove_workspace_folder,
        { buffer = ev.buf, desc = "[L]SP: [w]orkspace [r]emove folder" }
      )
      vim.keymap.set("n", "<Space>lwl", function()
        print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
      end, { buffer = ev.buf, desc = "[L]SP: [w]orkspace [l]ist folders" })
      vim.keymap.set("n", "<Space>D", vim.lsp.buf.type_definition, { buffer = ev.buf, desc = "LSP: type [D]efinition" })
      vim.keymap.set("n", "<Space>R", vim.lsp.buf.rename, { buffer = ev.buf, desc = "LSP: [R]ename" })
      vim.keymap.set(
        { "n", "v" },
        "<Space>A",
        vim.lsp.buf.code_action,
        { buffer = ev.buf, desc = "LSP: code [A]ction" }
      )
      vim.keymap.set("n", "gr", vim.lsp.buf.references, { buffer = ev.buf, desc = "LSP: [g]oto [r]eferences" })
      vim.keymap.set("n", "<space>F", function()
        vim.lsp.buf.format({ async = true })
      end, { buffer = ev.buf, desc = "LSP: async [F]ormat" })
    end,
  })
end

return M

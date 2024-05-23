local M = {}
local lspconfig = require("lspconfig")

--- Toggle inlay hints
---@param bufnr integer buffer number
function M.toggle_inlay_hints(bufnr)
  vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled())
end

function M.setup()
  local capabilities = vim.lsp.protocol.make_client_capabilities()
  vim.tbl_deep_extend("force", capabilities, require("cmp_nvim_lsp").default_capabilities())

  local function on_attach(client, bufnr)
    if client.supports_method("textDocument/inlayHint") then
      vim.lsp.inlay_hint.enable(true)
    end

    -- TODO: support toggling code lens on and off
    if client.supports_method("textDocument/codeLens") then
      vim.lsp.codelens.refresh()
      vim.api.nvim_create_autocmd({ "BufEnter", "CursorHold", "InsertLeave" }, {
        buffer = bufnr,
        callback = function()
          vim.lsp.codelens.refresh({ bufnr = 0 })
        end,
      })
    end

    if client.server_capabilities.documentSymbolProvider then
      require("nvim-navic").attach(client, bufnr)
    end

    require("config.statusline").init_lsp_progress()
  end

  local lsp_configs = {
    basedpyright = {
      on_attach = on_attach,
      capabilities = capabilities,
      settings = {
        basedpyright = {
          analysis = {
            diagnosticSeverityOverrides = {
              reportUnusedCallResult = "information",
              reportUnusedExpression = "information",
              reportUnknownMemberType = "none",
              reportUnknownLambdaType = "none",
              reportUnknownParameterType = "none",
              reportMissingParameterType = "none",
              reportUnknownVariableType = "none",
              reportUnknownArgumentType = "none",
              reportAny = "none",
            },
          },
        },
      },
    },
    pyright = false and {
      on_attach = on_attach,
      capabilities = capabilities,
      settings = {
        python = {
          analysis = {
            diagnosticSeverityOverrides = {
              reportGeneralTypeIssues = "information",
              reportPrivateImportUsage = "information",
              reportOptionalOperand = "information",
              reportOptionalSubscript = "information",
              reportOptionalMemberAccess = "information",
            },
          },
        },
      },
    },
    pylyzer = false,
    ruff_lsp = true,
    jinja_lsp = true,
    ast_grep = false,
    jsonls = true,
    clojure_lsp = true,
    biome = false,
    dockerls = true,
    jdtls = true,
    clangd = true,
    terraformls = true,
    rust_analyzer = {
      on_attach = on_attach,
      capabilities = capabilities,
      settings = {
        ["rust-analyzer"] = {
          cargo = {
            allFeatures = true,
            loadOutDirsFromCheck = true,
            runBuildScripts = true,
          },
          checkOnSave = {
            allFeatures = true,
            command = "clippy",
            extraArgs = { "--no-deps" },
          },
          procMacro = {
            enable = true,
            ignored = {
              ["async-trait"] = { "async_trait" },
              ["napi-derive"] = { "napi" },
              ["async-recursion"] = { "async_recursion" },
            },
          },
        },
      },
    },
    tsserver = true,
    vimls = true,
    ansiblels = true,
    helm_ls = {
      on_attach = on_attach,
      capabilities = capabilities,
      settings = {
        ["helm-ls"] = {
          yamlls = {
            path = "yaml-language-server",
          },
        },
      },
    },
    elixirls = true,
    ocamllsp = {
      on_attach = on_attach,
      capabilities = capabilities,
      settings = {
        codelens = { enable = true },
      },
    },
    yamlls = {
      on_attach = on_attach,
      capabilities = capabilities,
      settings = {
        yaml = {
          schemaStore = {
            enable = true,
            url = "https://www.schemastore.org/api/json/catalog.json",
          },
          keyOrdering = false,
          schemas = {
            kubernetes = "*.yaml",
            ["http://json.schemastore.org/github-workflow"] = ".github/workflows/*",
            ["http://json.schemastore.org/github-action"] = ".github/action.{yml,yaml}",
            ["http://json.schemastore.org/ansible-stable-2.9"] = "roles/tasks/*.{yml,yaml}",
            ["http://json.schemastore.org/prettierrc"] = ".prettierrc.{yml,yaml}",
            ["http://json.schemastore.org/kustomization"] = "kustomization.{yml,yaml}",
            ["http://json.schemastore.org/ansible-playbook"] = "*play*.{yml,yaml}",
            ["http://json.schemastore.org/chart"] = "Chart.{yml,yaml}",
            ["https://json.schemastore.org/dependabot-v2"] = ".github/dependabot.{yml,yaml}",
            ["https://json.schemastore.org/gitlab-ci"] = "*gitlab-ci*.{yml,yaml}",
            ["https://raw.githubusercontent.com/OAI/OpenAPI-Specification/main/schemas/v3.1/schema.json"] = "*api*.{yml,yaml}",
            ["https://raw.githubusercontent.com/compose-spec/compose-spec/master/schema/compose-spec.json"] = "*docker-compose*.{yml,yaml}",
            ["https://raw.githubusercontent.com/argoproj/argo-workflows/master/api/jsonschema/schema.json"] = "*flow*.{yml,yaml}",
          },
        },
      },
    },
    gopls = {
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
            test = true,
            upgrade_dependency = false,
            vendor = false,
          },
        },
      },
    },
    lua_ls = function()
      local runtime_path = vim.split(package.path, ";", {})
      table.insert(runtime_path, "lua/?.lua")
      table.insert(runtime_path, "lua/?/init.lua")

      lspconfig.lua_ls.setup({
        on_attach = on_attach,
        capabilities = capabilities,
        settings = {
          Lua = {
            codeLens = {
              enable = true,
            },
            completion = {
              callSnippet = "Replace",
            },
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
    end,
  }

  --- init LSP configurations
  --- lsp_configs is a table whose key is the lsp name and value is one of:
  --- boolean: use default configuration
  --- table: pass as config to lspconfig.setup
  --- function: to be called for lsp server configuration
  ---@param configs table lsp configurations
  local function init_configs(configs, default_config)
    local config = default_config or { on_attach = on_attach, capabilities = capabilities }
    for k, v in pairs(configs) do
      if type(v) == "boolean" then
        -- default configuration
        if v then
          lspconfig[k].setup(config)
        end
      elseif type(v) == "table" then
        -- custom configuration
        lspconfig[k].setup(v)
      elseif type(v) == "function" then
        v()
      else
        -- ivalid configuration
        print("Error: invalid LSP configuration: ", k, v)
      end
    end
  end

  local default_config = { on_attach = on_attach, capabilities = capabilities }
  init_configs(lsp_configs, default_config)

  -- stylua: ignore
  vim.api.nvim_create_autocmd("LspAttach", {
    group = vim.api.nvim_create_augroup("UserLspConfig", {}),
    callback = function(ev)
      -- Enable completion triggered by <c-x><c-o>
      vim.bo[ev.buf].omnifunc = "v:lua.vim.lsp.omnifunc"

      -- Buffer local mappings.
      -- See `:help vim.lsp.*` for documentation on any of the below functions
      vim.keymap.set("n", "<Leader>cgd", vim.lsp.buf.declaration, { buffer = ev.buf, desc = "LSP: Go To Declaration" })
      vim.keymap.set("n", "gd", vim.lsp.buf.definition, { buffer = ev.buf, desc = "LSP: [g]oto [d]efinition" })
      vim.keymap.set("n", "gi", vim.lsp.buf.implementation, { buffer = ev.buf, desc = "LSP: [g]oto [i]mplementation" })
      vim.keymap.set("n", "K", vim.lsp.buf.hover, { buffer = ev.buf, desc = "LSP: hover" })
      vim.keymap.set("i", "<C-s>", vim.lsp.buf.signature_help, { buffer = ev.buf, desc = "LSP: signature help" })
      vim.keymap.set("n", "<leader>lwa", vim.lsp.buf.add_workspace_folder, { buffer = ev.buf, desc = "[L]SP: [w]orkspace [a]dd folder" })
      vim.keymap.set("n", "<leader>lwr", vim.lsp.buf.remove_workspace_folder, { buffer = ev.buf, desc = "[L]SP: [w]orkspace [r]emove folder" })
      vim.keymap.set("n", "<leader>lwl", function() print(vim.inspect(vim.lsp.buf.list_workspace_folders())) end, { buffer = ev.buf, desc = "[L]SP: [w]orkspace [l]ist folders" })
      vim.keymap.set("n", "<leader>ld", vim.lsp.buf.type_definition, { buffer = ev.buf, desc = "LSP: type [D]efinition" })
      vim.keymap.set("n", "crn", vim.lsp.buf.rename, { buffer = ev.buf, desc = "LSP: [R]ename" })
      vim.keymap.set("n", "<leader>co", function()
        vim.lsp.buf.code_action({
          apply = true,
          context = {
            only = { "source.organizeImports" },
            diagnostics = {},
          },
        })
      end, { desc = "Organize Imports" })
      vim.keymap.set({ "n", "v" }, "crr", vim.lsp.buf.code_action, { buffer = ev.buf, desc = "LSP: code [A]ction" })
      vim.keymap.set({ "n", "v" }, "crR", function()
        vim.lsp.buf.code_action({
          context = {
            only = {
              "source",
            },
            diagnostics = {},
          },
        })
      end, { buffer = ev.buf, desc = "LSP: code [A]ction" })
      vim.keymap.set("n", "gr", vim.lsp.buf.references, { buffer = ev.buf, desc = "LSP: [g]oto [r]eferences" })
      vim.keymap.set("n", "<leader>F", function()
        vim.lsp.buf.format({ async = true })
      end, { buffer = ev.buf, desc = "LSP: async [F]ormat" })

      vim.keymap.set("n", "<leader>ll", function() M.toggle_inlay_hints(0) end, { buffer =ev.buf, desc = "LSP: Toggle inlay hints" })
      vim.keymap.set("n", "<leader>cr", vim.lsp.codelens.refresh, { desc = "Refresh Codelens" })
      vim.keymap.set({ "n", "v" }, "<leader>cR", vim.lsp.codelens.run, { desc = "Run Codelens" })
    end,
  })
end

return M

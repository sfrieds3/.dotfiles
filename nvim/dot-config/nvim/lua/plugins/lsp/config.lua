local M = {}
local lspconfig = require("lspconfig")

--- Toggle inlay hints
---@param bufnr integer buffer number
function M.toggle_inlay_hints(bufnr)
  vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled())
end

function M.setup()
  local capabilities = vim.lsp.protocol.make_client_capabilities()
  local err, _ = pcall(require, "blink.cmp")
  if err == nil then
    capabilities = require("blink.cmp").get_lsp_capabilities(capabilities)
  end

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

    require("config.statusline").init_lsp_progress()
  end

  local servers = {
    ansiblels = true,
    bashls = true,
    clojure_lsp = true,
    cssls = true,
    dockerls = true,
    elixirls = true,
    jdtls = true,
    lexical = true,
    ltex = false,
    marksman = true,
    ruff = true,
    taplo = true,
    terraformls = true,
    ts_ls = false,
    jinja_lsp = true,
    volar = {
      filetypes = { "typescript", "javascript", "javascriptreact", "typescriptreact", "vue" },
      init_options = {
        vue = {
          -- use embedded ts_ls
          hybridMode = false,
        },
      },
    },
    zls = true,

    basedpyright = {
      on_attach = on_attach,
      capabilities = capabilities,
      settings = {
        basedpyright = {
          -- using ruff organize imports
          -- disableOrganizeInputs = true,
          analysis = {
            -- ignore = { "*" },
            diagnosticSeverityOverrides = {
              reportUnusedCallResult = "none",
              reportUnusedExpression = "information",
              reportIncompatibleMethodOverride = "information",
              reportUnusedVariable = "information",
              reportImplicitOverride = "none",
              reportAny = "none",
              reportUntypedClassDecorator = "none",
              reportUntypedFunctionDecorator = "none",
              reportUnknownMemberType = "none",
              reportUnknownLambdaType = "none",
              reportUnknownParameterType = "none",
              reportMissingParameterType = "none",
              reportMissingTypeArgument = "none",
              reportUnknownVariableType = "none",
              reportUnknownArgumentType = "none",
              reportShadowedImports = "none",
            },
          },
        },
      },
    },

    clangd = {
      init_options = { clangdFileStatus = true },
    },

    ocamllsp = {
      on_attach = on_attach,
      capabilities = capabilities,
      settings = {
        codelens = { enable = true },
        inlayHints = { enable = true },
      },
    },

    helm_ls = {
      settings = {
        ["helm-ls"] = {
          yamlls = {
            path = "yaml-language-server",
          },
        },
      },
    },

    jsonls = {
      settings = {
        json = {
          schemas = require("schemastore").json.schemas(),
          validate = { enable = true },
        },
      },
    },

    yamlls = {
      settings = {
        json = {
          schemas = require("schemastore").json.schemas(),
          validate = { enable = true },
        },
      },
    },

    gopls = {
      on_attach = on_attach,
      capabilities = capabilities,
      settings = {
        gopls = {
          hints = {
            assignVariableTypes = true,
            compositeLiteralFields = true,
            compositeLiteralTypes = true,
            constatnsValues = true,
            functionTypeParameters = true,
            parameterNames = true,
            rangeVariableTypes = true,
          },
          staticcheck = true,
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
            hint = {
              enable = true,
            },
            codeLens = {
              enable = true,
            },
            completion = {
              callSnippet = "Replace",
            },
            runtime = {
              version = "LuaJIT",
              path = runtime_path,
            },
            diagnostics = {
              globals = { "vim" },
            },
            workspace = {
              library = vim.api.nvim_get_runtime_file("", true),
              checkThirdParty = false,
            },
            telemetry = {
              enable = false,
            },
          },
        },
      })
    end,
  }

  --- init LSP configurations
  --- servers is a table whose key is the lsp name and value is one of:
  --- boolean: use default configuration
  --- table: pass as config to lspconfig.setup
  --- function: to be called for lsp server configuration
  ---@param configs table lsp configurations
  local function init_configs(configs, default_config)
    local config = default_config or { on_attach = on_attach, capabilities = capabilities }
    for server, server_config in pairs(configs) do
      if type(server_config) == "boolean" then
        -- default configuration
        if server_config then
          lspconfig[server].setup(config)
        end
      elseif type(server_config) == "table" then
        -- custom configuration
        server_config = vim.tbl_deep_extend("keep", server_config, default_config)
        lspconfig[server].setup(server_config)
      elseif type(server_config) == "function" then
        server_config()
      else
        -- ivalid configuration
        print("Error: invalid LSP configuration: ", server, server_config)
      end
    end
  end

  local default_config = { on_attach = on_attach, capabilities = capabilities }
  init_configs(servers, default_config)

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
      -- vim.keymap.set("n", "<leader>F", function()
      --   vim.lsp.buf.format({ async = true })
      -- end, { buffer = ev.buf, desc = "LSP: async [F]ormat" })

      vim.keymap.set("n", "<leader>ll", function() M.toggle_inlay_hints(0) end, { buffer =ev.buf, desc = "LSP: Toggle inlay hints" })
      vim.keymap.set("n", "<leader>cr", vim.lsp.codelens.refresh, { desc = "Refresh Codelens" })
      vim.keymap.set({ "n", "v" }, "<leader>cR", vim.lsp.codelens.run, { desc = "Run Codelens" })
    end,
  })
end

return M

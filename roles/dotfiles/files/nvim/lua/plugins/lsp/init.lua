return {
  {
    "neovim/nvim-lspconfig",
    event = { "BufReadPre", "BufNewFile" },
    config = function()
      local ensure_installed = { "jsonls", "lua_ls", "pyright", "tsserver" }
      require("mason-lspconfig").setup({ ensure_installed = ensure_installed })
      require("plugins.lsp.config").setup()
    end,
  },
  {
    "williamboman/mason.nvim",
    cmd = { "Mason" },
    opts = {
      providers = { "mason.providers.client", "mason.providers.registry-api" },
    },
    dependencies = {
      "williamboman/mason-lspconfig.nvim",
      "nvim-telescope/telescope.nvim",
    },
  },
  {
    "SmiteshP/nvim-navic",
    event = "LspAttach",
    config = function()
      vim.g.navic_silence = true
      require("nvim-navic").setup({ separator = " ", highlight = true, depth_limit = 5 })
    end,
  },
  {
    "kosayoda/nvim-lightbulb",
    enabled = false,
    opts = {
      autocmd = { enabled = true },
    },
  },
  {
    "scalameta/nvim-metals",
    dependencies = {
      "nvim-lua/plenary.nvim",
      {
        "mfussenegger/nvim-dap",
        config = function(self, opts)
          -- Debug settings if you're using nvim-dap
          local dap = require("dap")

          dap.configurations.scala = {
            {
              type = "scala",
              request = "launch",
              name = "RunOrTest",
              metals = {
                runType = "runOrTestFile",
                --args = { "firstArg", "secondArg", "thirdArg" }, -- here just as an example
              },
            },
            {
              type = "scala",
              request = "launch",
              name = "Test Target",
              metals = {
                runType = "testTarget",
              },
            },
          }
        end,
      },
    },
    ft = { "scala", "sbt", "java" },
    opts = function()
      local metals_config = require("metals").bare_config()

      -- Example of settings
      metals_config.settings = {
        showImplicitArguments = true,
        excludedPackages = { "akka.actor.typed.javadsl", "com.github.swagger.akka.javadsl" },
      }

      metals_config.capabilities = require("cmp_nvim_lsp").default_capabilities()

      metals_config.on_attach = function(client, bufnr)
        require("metals").setup_dap()
      end

      return metals_config
    end,
    config = function(self, metals_config)
      local nvim_metals_group = vim.api.nvim_create_augroup("nvim-metals", { clear = true })
      vim.api.nvim_create_autocmd("FileType", {
        pattern = self.ft,
        callback = function()
          require("metals").initialize_or_attach(metals_config)
        end,
        group = nvim_metals_group,
      })
    end,
  },
  {
    "linrongbin16/lsp-progress.nvim",
    event = "LspAttach",
    opts = {},
  },
}

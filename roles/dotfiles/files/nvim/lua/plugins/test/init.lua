---@param config {args?:string[]|fun():string[]?}
local function get_args(config)
  local args = type(config.args) == "function" and (config.args() or {}) or config.args or {}
  config = vim.deepcopy(config)
  ---@cast args string[]
  config.args = function()
    local new_args = vim.fn.input("Run with args: ", table.concat(args, " ")) --[[@as string]]
    return vim.split(vim.fn.expand(new_args) --[[@as string]], " ")
  end
  return config
end

return {
  {
    "folke/which-key.nvim",
    optional = true,
    opts = {
      defaults = {
        ["<leader>D"] = { name = "+Debug" },
        ["<leader>T"] = { name = "+Test" },
      },
    },
  },
  {
    "nvim-neotest/neotest",
    dependencies = {
      "nvim-neotest/neotest-python",
      "nvim-neotest/neotest-go",
      "rouge8/neotest-rust",
    },
    opts = {
      adapters = {
        ["neotest-python"] = {},
        ["neotest-go"] = {},
        ["neotest-rust"] = {},
      },
      status = { virtual_text = true },
      output = { open_on_run = true },
      quickfix = {
        open = function()
          require("trouble").open({ mode = "quickfix", focus = false })
        end,
      },
    },
    config = function(_, opts)
      local neotest_ns = vim.api.nvim_create_namespace("neotest")
      vim.diagnostic.config({
        virtual_text = {
          format = function(diagnostic)
            -- Replace newline and tab characters with space for more compact diagnostics
            local message = diagnostic.message:gsub("\n", " "):gsub("\t", " "):gsub("%s+", " "):gsub("^%s+", "")
            return message
          end,
        },
      }, neotest_ns)

      opts.consumers = opts.consumers or {}
      -- Refresh and auto close trouble after running tests
      ---@type neotest.Consumer
      opts.consumers.trouble = function(client)
        client.listeners.results = function(adapter_id, results, partial)
          if partial then
            return
          end
          local tree = assert(client:get_position(nil, { adapter = adapter_id }))

          local failed = 0
          for pos_id, result in pairs(results) do
            if result.status == "failed" and tree:get_key(pos_id) then
              failed = failed + 1
            end
          end
          vim.schedule(function()
            local trouble = require("trouble")
            if trouble.is_open() then
              trouble.refresh()
              if failed == 0 then
                trouble.close()
              end
            end
          end)
          return {}
        end
      end

      if opts.adapters then
        local adapters = {}
        for name, config in pairs(opts.adapters or {}) do
          if type(name) == "number" then
            if type(config) == "string" then
              config = require(config)
            end
            adapters[#adapters + 1] = config
          elseif config ~= false then
            local adapter = require(name)
            if type(config) == "table" and not vim.tbl_isempty(config) then
              local meta = getmetatable(adapter)
              if adapter.setup then
                adapter.setup(config)
              elseif meta and meta.__call then
                adapter(config)
              else
                error("Adapter " .. name .. " does not support setup")
              end
            end
            adapters[#adapters + 1] = adapter
          end
        end
        opts.adapters = adapters
      end

      require("neotest").setup(opts)
    end,
    -- stylua: ignore
    keys = {
      { "<leader>Tt", function() require("neotest").run.run(vim.fn.expand("%")) end, desc = "Run File" },
      { "<leader>TT", function() require("neotest").run.run(vim.loop.cwd()) end, desc = "Run All Test Files" },
      { "<leader>Tr", function() require("neotest").run.run() end, desc = "Run Nearest" },
      { "<leader>Ts", function() require("neotest").summary.toggle() end, desc = "Toggle Summary" },
      { "<leader>To", function() require("neotest").output.open({ enter = true, auto_close = true }) end, desc = "Show Output" },
      { "<leader>TO", function() require("neotest").output_panel.toggle() end, desc = "Toggle Output Panel" },
      { "<leader>TS", function() require("neotest").run.stop() end, desc = "Stop" },
    },
  },
  {
    "mfussenegger/nvim-dap",

    config = function()
      vim.api.nvim_set_hl(0, "DapStoppedLine", { default = true, link = "Visual" })
    end,

    dependencies = {
      {
        "mfussenegger/nvim-dap-python",
      -- stylua: ignore
      keys = {
        { "<leader>DPt", function() require('dap-python').test_method() end, desc = "Debug Method", ft = "python" },
        { "<leader>DPc", function() require('dap-python').test_class() end, desc = "Debug Class", ft = "python" },
      },
        config = function()
          local path = require("mason-registry").get_package("debugpy"):get_install_path()
          require("dap-python").setup(path .. "/venv/bin/python")

          require("dap-python").resolve_python = function()
            local on_exit = function(obj)
              return obj.stdout:gsub("\n", "")
            end
            vim.system({ "which", "python3" }, { text = true }, on_exit):wait()
          end
        end,
      },
      {
        "leoluz/nvim-dap-go",
      -- stylua: ignore
        keys = {
          { "<leader>DPt", function() require("dap-go").debug_test() end, desc = "Debug Method", ft = "go" },
        },
        config = true,
      },
      {
        "rcarriga/nvim-dap-ui",
      -- stylua: ignore
      keys = {
        { "<leader>Du", function() require("dapui").toggle({ }) end, desc = "Dap UI" },
        { "<leader>De", function() require("dapui").eval() end, desc = "Eval", mode = {"n", "v"} },
      },
        opts = {},
        config = function(_, opts)
          -- require("dap.ext.vscode").load_launchjs()
          local dap = require("dap")
          local dapui = require("dapui")
          dapui.setup(opts)
          dap.listeners.after.event_initialized["dapui_config"] = function()
            dapui.open({})
          end
          dap.listeners.before.event_terminated["dapui_config"] = function()
            dapui.close({})
          end
          dap.listeners.before.event_exited["dapui_config"] = function()
            dapui.close({})
          end
        end,
      },
      {
        "theHamsta/nvim-dap-virtual-text",
        config = true,
      },
      {
        "nvim-telescope/telescope-dap.nvim",
        config = function()
          require("telescope").load_extension("dap")
        end,
      },
    },

    -- stylua: ignore
    keys = {
      { "<leader>Td", function() require("neotest").run.run({ strategy = "dap" }) end, desc = "Debug Nearest" },
      { "<leader>DB", function() require("dap").set_breakpoint(vim.fn.input("Breakpoint condition: ")) end, desc = "Breakpoint Condition" },
      { "<leader>Db", function() require("dap").toggle_breakpoint() end, desc = "Toggle Breakpoint" },
      { "<leader>Dc", function() require("dap").continue() end, desc = "Continue" },
      { "<leader>Da", function() require("dap").continue({ before = get_args }) end, desc = "Run with Args" },
      { "<leader>DC", function() require("dap").run_to_cursor() end, desc = "Run to Cursor" },
      { "<leader>Dg", function() require("dap").goto_() end, desc = "Go to line (no execute)" },
      { "<F7>", function() require("dap").step_into() end, desc = "Step Into" },
      { "<leader>Dj", function() require("dap").down() end, desc = "Down" },
      { "<leader>Dk", function() require("dap").up() end, desc = "Up" },
      { "<leader>Dl", function() require("dap").run_last() end, desc = "Run Last" },
      { "<F9>", function() require("dap").step_out() end, desc = "Step Out" },
      { "<F8>", function() require("dap").step_over() end, desc = "Step Over" },
      { "<leader>Dp", function() require("dap").pause() end, desc = "Pause" },
      { "<leader>Dr", function() require("dap").repl.toggle() end, desc = "Toggle REPL" },
      { "<leader>Ds", function() require("dap").session() end, desc = "Session" },
      { "<leader>DT", function() require("dap").terminate() end, desc = "Terminate" },
      { "<F3>", function() require("dap").terminate() end, desc = "Terminate" },
      { "<leader>Dw", function() require("dap.ui.widgets").hover() end, desc = "Widgets" },
    },
  },
  {
    "jay-babu/mason-nvim-dap.nvim",
    dependencies = "mason.nvim",
    cmd = { "DapInstall", "DapUninstall" },
    opts = {
      automatic_installation = true,
      handlers = {},
      ensure_installed = {},
    },
  },
}

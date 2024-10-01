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
    opts = {
      spec = {
        { "<localleader>d", group = "debug" },
        { "<localleader>dt", group = "debug test" },
        { "<localleader>t", group = "test" },
      },
    },
  },
  {
    "nvim-neotest/neotest",
    dependencies = {
      "nvim-neotest/nvim-nio",
      "antoinemadec/FixCursorHold.nvim",
      "nvim-lua/plenary.nvim",
      "nvim-neotest/neotest-python",
      "nvim-neotest/neotest-go",
      "rouge8/neotest-rust",
      "nvim-lua/plenary.nvim",
    },
    opts = {
      adapters = {
        ["neotest-python"] = {
          dap = { justMyCode = false },
        },
        ["neotest-go"] = {},
        ["rustaceanvim.neotest"] = {},
      },
      status = { virtual_text = true },
      output = { enabled = true, open_on_run = false },
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
      { "<localleader>tt", function() require("neotest").run.run() end, desc = "Run Nearest" },
      { "<localleader>tf", function() require("neotest").run.run(vim.fn.expand("%")) end, desc = "Run File" },
      { "<localleader>tF", function() require("neotest").run.run(vim.uv.cwd()) end, desc = "Run All Test Files" },
      { "<localleader>ts", function() require("neotest").summary.toggle() end, desc = "Toggle Summary" },
      { "<localleader>to", function() require("neotest").output.open({ enter = true, auto_close = true }) end, desc = "Show Output" },
      { "<localleader>tO", function() require("neotest").output_panel.toggle() end, desc = "Toggle Output Panel" },
      { "<localleader>tS", function() require("neotest").run.stop() end, desc = "Stop" },
    },
  },
  {
    "mfussenegger/nvim-dap",

    config = function()
      vim.api.nvim_set_hl(0, "DapStoppedLine", { default = true, link = "Visual" })
      vim.fn.sign_define("DapBreakpointCondition", {
        text = "",
        texthl = "DiagnosticSignError",
      })
      vim.fn.sign_define("DapBreakpoint", {
        text = "",
        texthl = "DiagnosticSignError",
      })
      vim.fn.sign_define("DapStopped", {
        text = "",
        texthl = "DiagnosticSignInfo",
        linehl = "DapStoppedLine",
      })
    end,

    dependencies = {
      {
        "mfussenegger/nvim-dap-python",
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
          { "<localleader>dtt", function() require("dap-go").debug_test() end, desc = "Debug Method", ft = "go" },
        },
        config = true,
      },
      {
        "rcarriga/nvim-dap-ui",
        -- stylua: ignore
        keys = {
          { "<localleader>uu", function() require("dapui").toggle({reset = true }) end, desc = "Toggle Dap UI" },
          { "<localleader>ue", function() require("dapui").eval() end, desc = "Dap UI Eval", mode = {"n", "v"} },
          { "<localleader>uw", function() require("dapui").elements.watches.add() end, desc = "Dap UI Add watch" },
          { "<localleader>uW", function() require("dapui").elements.watches.remove() end, desc = "Dap UI Add watch" },
          ---@diagnostic disable-next-line: missing-parameter
          { "<localleader>uf", function() require("dapui").float_element() end, desc = "Dap UI Float Element" },
          { "<localleader>us", function() require("dapui").float_element("scopes") end, desc = "Dap UI Float Scopes" },
          { "<localleader>ur", function() require("dapui").float_element("repl") end, desc = "Dap UI Float Repl" },
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
        dependencies = {
          "nvim-neotest/nvim-nio",
        },
      },
      {
        "theHamsta/nvim-dap-virtual-text",
        config = true,
      },
    },

    -- stylua: ignore
    keys = {
      ---@diagnostic disable-next-line: missing-fields
      { "<localleader>td", function() require("neotest").run.run({ strategy = "dap" }) end, desc = "Debug Nearest" },
      { "<localleader>B", function() require("dap").set_breakpoint(vim.fn.input("Breakpoint condition: ")) end, desc = "Breakpoint Condition" },
      { "<localleader>b", function() require("dap").toggle_breakpoint() end, desc = "Toggle Breakpoint" },
      { "<localleader>dc", function() require("dap").continue() end, desc = "Continue" },
      { "<localleader>da", function() require("dap").continue({ before = get_args }) end, desc = "Run with Args" },
      { "<localleader>dC", function() require("dap").run_to_cursor() end, desc = "Run to Cursor" },
      { "<localleader>dg", function() require("dap").goto_() end, desc = "Go to line (no execute)" },
      { "<F7>", function() require("dap").step_into() end, desc = "Step Into" },
      { "<localleader>dj", function() require("dap").down() end, desc = "Down" },
      { "<localleader>dk", function() require("dap").up() end, desc = "Up" },
      { "<localleader>dl", function() require("dap").run_last() end, desc = "Run Last" },
      { "<F9>", function() require("dap").step_out() end, desc = "Step Out" },
      { "<F8>", function() require("dap").step_over() end, desc = "Step Over" },
      { "<localleader>dp", function() require("dap").pause() end, desc = "Pause" },
      { "<localleader>dr", function() require("dap").repl.toggle() end, desc = "Toggle REPL" },
      { "<localleader>ds", function() require("dap").session() end, desc = "Session" },
      { "<localleader>dT", function() require("dap").terminate() end, desc = "Terminate" },
      { "<F2>", function() require("dap").terminate() end, desc = "Terminate" },
      { "<localleader>dw", function() require("dap.ui.widgets").hover() end, desc = "Widgets" },
      ---@diagnostic disable-next-line: missing-fields
      { "<space>?", function() require("dapui").eval(nil, { enter = true }) end },
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

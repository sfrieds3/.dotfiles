return {
  "folke/snacks.nvim",
  priority = 1000,
  lazy = false,
  opts = {
    bigfile = { enabled = true },
    debug = { enabled = true },
    picker = {
      layout = { preset = "vertical" },
      win = {
        preview = {
          wo = { number = false },
        },
      },
      formatters = {
        file = {
          filename_first = true,
        },
      },
    },
    indent = { enabled = false },
    bufdelete = { enabled = true },
    words = { enabled = true },
  },
  init = function()
    vim.api.nvim_create_autocmd("User", {
      pattern = "VeryLazy",
      callback = function()
        -- Debug
        _G.dd = function(...)
          Snacks.debug.inspect(...)
        end
        _G.bt = function()
          Snacks.debug.backtrace()
        end
        vim.print = _G.dd

        -- Toggle
        Snacks.toggle
          .option("conceallevel", { off = 0, on = vim.o.conceallevel > 0 and vim.o.conceallevel or 2 })
          :map("<leader>uc")
        Snacks.toggle.option("background", { off = "light", on = "dark", name = "Dark Background" }):map("<leader>ub")
        Snacks.toggle.inlay_hints():map("<C-h>")
        Snacks.toggle.indent():map("<M-i>")
      end,
    })
  end,
  -- stylua: ignore
  keys = {
    { "<localleader>q", function() Snacks.bufdelete() end, desc = "BufDelete" },
    { "<C-j>", function() Snacks.words.jump(1, true) end, desc = "Words Jump Previous" },
    { "<C-k>", function() Snacks.words.jump(-1, true) end, desc = "Words Jump Next" },

    { "<leader><leader>", function() Snacks.picker.buffers() end, desc = "Buffers" },
    { "<leader>,", function() Snacks.picker.smart() end, desc = "Smart" },
    { "<leader>\\", function() Snacks.picker.explorer() end, desc = "Explorer" },
 --    { "<leader>/", function() Snacks.picker.grep() end, desc = "Grep" },
 --    { "<leader>:", function() Snacks.picker.command_history() end, desc = "Command History" },
 --    { "<leader><space>", function() Snacks.picker.files() end, desc = "Find Files" },
 --    -- find
 --    { "<leader>fb", function() Snacks.picker.buffers() end, desc = "Buffers" },
 --    { "<leader>fc", function() Snacks.picker.files({ cwd = vim.fn.stdpath("config") }) end, desc = "Find Config File" },
 --    { "<leader>ff", function() Snacks.picker.files() end, desc = "Find Files" },
 --    { "<leader>fg", function() Snacks.picker.git_files() end, desc = "Find Git Files" },
 --    { "<leader>fr", function() Snacks.picker.recent() end, desc = "Recent" },
 --    -- git
 --    { "<leader>gc", function() Snacks.picker.git_log() end, desc = "Git Log" },
 --    { "<leader>gs", function() Snacks.picker.git_status() end, desc = "Git Status" },
 --    -- Grep
 --    { "<leader>sb", function() Snacks.picker.lines() end, desc = "Buffer Lines" },
 --    { "<leader>sB", function() Snacks.picker.grep_buffers() end, desc = "Grep Open Buffers" },
 --    { "<leader>sg", function() Snacks.picker.grep() end, desc = "Grep" },
 --    { "<leader>sw", function() Snacks.picker.grep_word() end, desc = "Visual selection or word", mode = { "n", "x" } },
 --    -- search
 --    { '<leader>s"', function() Snacks.picker.registers() end, desc = "Registers" },
 --    { "<leader>sa", function() Snacks.picker.autocmds() end, desc = "Autocmds" },
 --    { "<leader>sc", function() Snacks.picker.command_history() end, desc = "Command History" },
 --    { "<leader>sC", function() Snacks.picker.commands() end, desc = "Commands" },
 --    { "<leader>sd", function() Snacks.picker.diagnostics() end, desc = "Diagnostics" },
 --    { "<leader>sh", function() Snacks.picker.help() end, desc = "Help Pages" },
 --    { "<leader>sH", function() Snacks.picker.highlights() end, desc = "Highlights" },
 --    { "<leader>sj", function() Snacks.picker.jumps() end, desc = "Jumps" },
 --    { "<leader>sk", function() Snacks.picker.keymaps() end, desc = "Keymaps" },
 --    { "<leader>sl", function() Snacks.picker.loclist() end, desc = "Location List" },
 --    { "<leader>sM", function() Snacks.picker.man() end, desc = "Man Pages" },
 --    { "<leader>sm", function() Snacks.picker.marks() end, desc = "Marks" },
 --    { "<leader>sR", function() Snacks.picker.resume() end, desc = "Resume" },
 --    { "<leader>sq", function() Snacks.picker.qflist() end, desc = "Quickfix List" },
 --    { "<leader>uC", function() Snacks.picker.colorschemes() end, desc = "Colorschemes" },
 --    { "<leader>qp", function() Snacks.picker.projects() end, desc = "Projects" },
 --    -- LSP
 --    { "gd", function() Snacks.picker.lsp_definitions() end, desc = "Goto Definition" },
 --    { "gr", function() Snacks.picker.lsp_references() end, nowait = true, desc = "References" },
 --    { "gI", function() Snacks.picker.lsp_implementations() end, desc = "Goto Implementation" },
 --    { "gy", function() Snacks.picker.lsp_type_definitions() end, desc = "Goto T[y]pe Definition" },
 --    { "<leader>ss", function() Snacks.picker.lsp_symbols() end, desc = "LSP Symbols" },
  }
,
}

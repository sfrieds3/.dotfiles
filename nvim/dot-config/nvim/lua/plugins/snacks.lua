local function live_grep_all_files()
  require("snacks").picker({
    finder = "grep",
    live = true,
    supports_live = true,
    regex = true,
    show_empty = true,
    cmd = "rg",
    args = {
      "--column",
      "--line-number",
      "--no-heading",
      "--color=always",
      "--smart-case",
      "--max-columns=4096",
      "--no-ignore",
      "--hidden",
    },
    title = "Live Grep All",
    layout = {
      preset = "select",
    },
    actions = {
      confirm = function(picker, item)
        picker:close()
        vim.schedule(function()
          Snacks.picker.files({
            cwd = item.file,
          })
        end)
      end,
    },
    transform = function(item)
      item.file = item.text
      item.dir = true
    end,
  })
end

return {
  "folke/snacks.nvim",
  priority = 1000,
  lazy = false,
  opts = {
    bigfile = {},
    debug = {},
    picker = {
      layout = { preset = "vertical" },
      win = {
        preview = {
          wo = {
            number = false,
            relativenumber = false,
          },
        },
      },
      formatters = {
        file = {
          filename_first = true,
          truncate = 120,
        },
      },
    },
    indent = { enabled = false },
    bufdelete = {},
    words = {},
    statuscolumn = {},
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
        Snacks.toggle.option("cursorline"):map("<localleader>c")
        Snacks.toggle.option("cursorcolumn"):map("<localleader>C")
      end,
    })
  end,
  -- stylua: ignore
  keys = {
    -- bufdelete
    { "<localleader>q", function() Snacks.bufdelete() end, desc = "BufDelete" },

    -- words
    { "<C-j>", function() Snacks.words.jump(1, true) end, desc = "Words Jump Previous" },
    { "<C-k>", function() Snacks.words.jump(-1, true) end, desc = "Words Jump Next" },

    -- picker
    { "<leader>,", function() Snacks.picker.smart() end, desc = "Smart" },
    { "<leader>\\", function() Snacks.picker.explorer() end, desc = "Explorer" },
    { "<leader>sb", function() Snacks.picker.lines() end, desc = "Buffer Lines [fuzzy]" },
    { "<leader>?", function() Snacks.picker.lines() end, desc = "Buffer Lines [fuzzy]" },
    { "<leader>/", function() Snacks.picker.lines({ matcher = { fuzzy = false } }) end, desc = "Buffer Lines [strict]" },
    { "<leader>sB", function() Snacks.picker.grep_buffers() end, desc = "Grep Open Buffers" },
    { "<leader>ff", function() Snacks.picker.files({layout = { preset = "ivy"} }) end, desc = "Find Files" },
    { "<leader>fF", function() Snacks.picker.files({layout = { preset = "ivy"}, hidden=true, ignored=true }) end, desc = "Find All Files" },
    { "<leader><leader>", function() Snacks.picker.buffers({layout = { preset = "ivy"} }) end, desc = "Buffers" },
    { "<leader>sr", function() Snacks.picker.grep({layout = { preview = 'main', preset = 'ivy' }, jump = { match = true } }) end, desc = "Grep" },
    { "<leader>rg", function() Snacks.picker.grep() end, desc = "Grep" },
    { "<leader>rG", function() live_grep_all_files() end, desc = "Live Grep in All Files" },
    { "<leader>gr", function() Snacks.picker.grep_word() end, desc = "Visual selection or word", mode = { "n", "x" } },
    { "<leader>:", function() Snacks.picker.command_history() end, desc = "Command History" },
    { "<leader>fb", function() Snacks.picker.buffers() end, desc = "Buffers" },
    { "<leader>fc", function() Snacks.picker.files({ cwd = vim.fn.stdpath("config") }) end, desc = "Find Config File" },
    { "<leader>U", function() Snacks.picker.undo() end, desc = "Undo (Changes)" },
    { "<leader>fg", function() Snacks.picker.git_files() end, desc = "Find Git Files" },
    { "<leader>fr", function() Snacks.picker.recent() end, desc = "Recent" },
    { "<leader>gc", function() Snacks.picker.git_log() end, desc = "Git Log" },
    { "<leader>gs", function() Snacks.picker.git_status() end, desc = "Git Status" },
    { "<leader>sg", function() Snacks.picker.grep() end, desc = "Grep" },
    { '<leader>sr"', function() Snacks.picker.registers() end, desc = "Registers" },
    { "<leader>sa", function() Snacks.picker.autocmds() end, desc = "Autocmds" },
    { "<leader>sc", function() Snacks.picker.command_history() end, desc = "Command History" },
    { "<leader>sC", function() Snacks.picker.commands() end, desc = "Commands" },
    { "<leader>sd", function() Snacks.picker.diagnostics() end, desc = "Diagnostics" },
    { "<leader>sh", function() Snacks.picker.help() end, desc = "Help Pages" },
    { "<leader>sH", function() Snacks.picker.highlights() end, desc = "Highlights" },
    { "<leader>sj", function() Snacks.picker.jumps() end, desc = "Jumps" },
    { "<leader>sk", function() Snacks.picker.keymaps() end, desc = "Keymaps" },
    { "<leader>sl", function() Snacks.picker.loclist() end, desc = "Location List" },
    { "<leader>sM", function() Snacks.picker.man() end, desc = "Man Pages" },
    { "<leader>sm", function() Snacks.picker.marks() end, desc = "Marks" },
    { "<leader>.", function() Snacks.picker.resume() end, desc = "Resume" },
    { "<leader>sq", function() Snacks.picker.qflist() end, desc = "Quickfix List" },
    { "<leader>uC", function() Snacks.picker.colorschemes() end, desc = "Colorschemes" },
    { "<leader>qp", function() Snacks.picker.projects() end, desc = "Projects" },
    { "<leader>gd", function() Snacks.picker.lsp_definitions() end, desc = "Goto Definition" },
    { "<leader>gR", function() Snacks.picker.lsp_references() end, nowait = true, desc = "References" },
    { "<leader>gI", function() Snacks.picker.lsp_implementations() end, desc = "Goto Implementation" },
    { "<leader>gy", function() Snacks.picker.lsp_type_definitions() end, desc = "Goto T[y]pe Definition" },
    { "<leader>ss", function() Snacks.picker.lsp_symbols() end, desc = "LSP Symbols" },
    { "<leader>sS", function() Snacks.picker.lsp_workspace_symbols() end, desc = "LSP Symbols" },
  }
,
}

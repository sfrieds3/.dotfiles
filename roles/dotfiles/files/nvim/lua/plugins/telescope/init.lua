local Utils = require("utils.telescope")

return {
  "nvim-telescope/telescope.nvim",

  cmd = "Telescope",

  config = function()
    require("plugins.telescope.config").setup()
  end,
  dependencies = {
    "nvim-lua/plenary.nvim",
    "nvim-lua/popup.nvim",
    "nvim-telescope/telescope-ui-select.nvim",
    "nvim-telescope/telescope-frecency.nvim",
    {
      "nvim-telescope/telescope-fzf-native.nvim",
      build = "make",
    },
    "Marskey/telescope-sg",
    "debugloop/telescope-undo.nvim",
  },

  keys = {
    {
      "<leader><leader>",
      "<cmd>Telescope buffers sort_mru=true sort_lastused=true theme=ivy<cr>",
      desc = "Buffers",
    },
    {
      "<leader>/",
      "<cmd>Telescope current_buffer_fuzzy_find theme=ivy<cr>",
      desc = "Curent Buffer Fuzzy Find",
    },
    {
      "<leader>gr",
      "<cmd>Telescope grep_string theme=ivy<cr>",
      mode = { "n", "v" },
      desc = "Grep String",
    },
    {
      "<leader>s:",
      "<cmd>Telescope search_history theme=dropdown previewer=false<cr>",
      desc = "Search History",
    },
    {
      "<leader>sH",
      "<cmd>Telescope highlights<cr>",
      desc = "Search History",
    },
    {
      "<leader>sc",
      "<cmd>Telescope command_history theme=dropdown previewer=false<cr>",
      desc = "Command History",
    },
    {
      "<Leader>.",
      "<cmd>Telescope resume<cr>",
      desc = "Telescope Resume",
    },
    {
      "<Leader>ff",
      function()
        Utils("project_files", { type = "custom" })
      end,
      desc = "Project files",
    },
    {
      "<Leader>fo",
      "<cmd>Telescope oldfiles theme=ivy",
      desc = "Old files",
    },
    {
      "<Leader>fr",
      function()
        local opts = require("telescope.themes").get_ivy()
        require("telescope").extensions.frecency.frecency(opts)
      end,
      desc = "Recent files",
    },
    { "<Leader>u", "<cmd>Telescope undo<cr>", desc = "Undo" },
    { "<Leader>sS", "<cmd>Telescope lsp_dynamic_workspace_symbols<cr>", desc = "Workspace Symbols" },
    { "<Leader>ss", "<cmd>Telescope lsp_document_symbols<cr>", desc = "Document symbols" },
    {
      "<Leader>gR",
      function()
        Utils("preview_lsp_references", { type = "custom" })
      end,
      desc = "LSP References",
    },
    {
      "<Leader>gD",
      function()
        Utils("preview_lsp_definitions", { type = "custom" })
      end,
      desc = "LSP Definitions",
    },
    {
      "<Leader>I",
      function()
        Utils("preview_lsp_incoming_calls", { type = "custom" })
      end,
      desc = "Incomming calls",
    },
    {
      "<Leader>O",
      function()
        Utils("preview_lsp_outgoing_calls", { type = "custom" })
      end,
      desc = "Outgiong calls",
    },
    {
      "<Leader>so",
      function()
        Utils("vim_options", { type = "custom" })
      end,
      desc = "Options",
    },
    {
      "<Leader>sw",
      function()
        Utils("wiki_search", { type = "custom" })
      end,
      desc = "Search Wiki",
    },
    {
      "<Leader>gw",
      function()
        Utils("grep_wiki", { type = "custom" })
      end,
      desc = "Grep Wiki",
    },
    {
      "<Leader>gw",
      function()
        Utils("live_grep", { type = "custom" })
      end,
      desc = "Live Grep",
    },
    {
      "<Leader>rg",
      function()
        local multi_rg = require("plugins.telescope.multi_rg")
        multi_rg()
      end,
      desc = "Telescope: multi [r]ip[g]rep",
    },
    {
      "<Leader>gp",
      function()
        Utils("live_grep_preview", { type = "custom" })
      end,
      desc = "Live Grep Preview",
    },
    {
      "<Leader>ga",
      function()
        Utils("live_grep_args", { type = "custom" })
      end,
      desc = "Live Grep Args",
    },
    {
      "<Leader>g/",
      function()
        Utils("grep_last_search", { type = "custom" })
      end,
      desc = "Grep Last Search",
    },
    {
      "<Leader>sp",
      function()
        require("telescope").extensions.neoclip.default()
      end,
      desc = "Telescope: neoclip",
    },
    { "<Leader>sk", "<cmd>Telescope keymaps<cr>", desc = "Keymaps" },
    {
      "<Leader>di",
      function()
        Utils("installed_plugins", { type = "custom" })
      end,
      desc = "Installed Plugins",
    },
    {
      "<Leader>gi",
      function()
        Utils("grep_installed_plugins", { type = "custom" })
      end,
      desc = "Installed Plugins",
    },
    {
      "<leader>sj",
      function()
        Utils("jumplist", { theme = "dropdown", winblend = 10, layout_config = { width = 0.5 } })
      end,
      desc = "Jumplist",
    },
    {
      "<leader>sm",
      function()
        Utils("marks", { theme = "dropdown", winblend = 10, layout_config = { width = 0.5 } })
      end,
      desc = "Marks",
    },
    {
      "<leader>sr",
      function()
        Utils("registers", { theme = "dropdown", winblend = 10, layout_config = { width = 0.5 } })
      end,
      desc = "Registers",
    },
    {
      "<leader>sh",
      function()
        Utils("help_tags", { theme = "dropdown", winblend = 10, layout_config = { width = 0.5 } })
      end,
      desc = "Registers",
    },
    {
      "<leader>sd",
      "<cmd>Telescope diagnostics bufnr=0<cr>",
      desc = "Buffer Diagnostics",
    },
    {
      "<leader>sD",
      "<cmd>Telescope diagnostics<cr>",
      desc = "Workspace Diagnostics",
    },
    { "<leader>sq", "<cmd>Telescope quickfix theme=ivy<cr>", desc = "Quickfix" },
    { "<leader>sl", "<cmd>Telescope loclist theme=ivy<cr>", desc = "Loclist" },
  },
}

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
    "nvim-telescope/telescope-frecency.nvim",
    {
      "nvim-telescope/telescope-fzf-native.nvim",
      build = "make",
    },
    "Marskey/telescope-sg",
    "debugloop/telescope-undo.nvim",
    "nvim-telescope/telescope-ui-select.nvim",
  },

  -- TODO: add keymap to search non-git files in a repo (<leader>fF)
  -- stylua: ignore
  keys = {
    { "<leader><leader>", "<cmd>Telescope buffers sort_mru=true sort_lastused=true theme=ivy<cr>", desc = "Buffers" },
    { "<leader>/", "<cmd>Telescope current_buffer_fuzzy_find theme=ivy<cr>", desc = "Curent Buffer Fuzzy Find" },
    { "<leader>gr", "<cmd>Telescope grep_string theme=ivy<cr>", mode = { "n", "v" }, desc = "Grep String" },
    { "<leader>s:", "<cmd>Telescope search_history theme=dropdown previewer=false<cr>", desc = "Search History" },
    { "<leader>sH", "<cmd>Telescope highlights<cr>", desc = "Search Highlights" },
    { "<leader>sc", "<cmd>Telescope command_history theme=dropdown previewer=false<cr>", desc = "Command History" },
    { "<leader>.", "<cmd>Telescope resume<cr>", desc = "Telescope Resume" },
    { "<leader>ff", function() Utils("project_files", { type = "custom" }) end, desc = "Project files" },
    { "<leader>fR", function() require("telescope.builtin").find_files({ cwd = vim.fn.expand('%:p:h') }) end , desc = "Find Related Files" },
    { "<leader>fo", "<cmd>Telescope oldfiles theme=ivy", desc = "Old files" },
    { "<leader>fF", function() Utils("find_all_files", { type = "custom" }) end, desc = "Find All Files" },
    { "<leader>sF", function() Utils("search_all_files", { type = "custom" }) end, desc = "Search All Files" },
    {
      "<leader>fr",
      function()
        local opts = require("telescope.themes").get_ivy()
        require("telescope").extensions.frecency.frecency(opts)
      end,
      desc = "Recent files",
    },
    { "<leader>u", "<cmd>Telescope undo<cr>", desc = "Undo" },
    { "<leader>sS", "<cmd>Telescope lsp_dynamic_workspace_symbols<cr>", desc = "Workspace Symbols" },
    { "<leader>ss", "<cmd>Telescope lsp_document_symbols<cr>", desc = "Document symbols" },
    { "<leader>gR", function() Utils("preview_lsp_references", { type = "custom" }) end, desc = "LSP References" },
    { "<leader>gD", function() Utils("preview_lsp_definitions", { type = "custom" }) end, desc = "LSP Definitions" },
    { "<leader>I", function() Utils("preview_lsp_incoming_calls", { type = "custom" }) end, desc = "Incomming calls" },
    { "<leader>O", function() Utils("preview_lsp_outgoing_calls", { type = "custom" }) end, desc = "Outgiong calls" },
    { "<leader>so", function() Utils("vim_options", { type = "custom" }) end, desc = "Options" },
    { "<leader>sw", function() Utils("wiki_search", { type = "custom" }) end, desc = "Search Wiki" },
    { "<leader>gW", function() Utils("grep_wiki", { type = "custom" }) end, desc = "Grep Wiki" },
    { "<leader>gg", function() Utils("live_grep", { type = "custom" }) end, desc = "Live Grep" },
    { "<leader>rg", function() require("plugins.telescope.multi_rg")() end, desc = "Telescope: multi [r]ip[g]rep" },
    { "<leader>gp", function() Utils("live_grep_preview", { type = "custom" }) end, desc = "Live Grep Preview" },
    { "<leader>ga", function() Utils("live_grep_args", { type = "custom" }) end, desc = "Live Grep Args" },
    { "<leader>g/", function() Utils("grep_last_search", { type = "custom" }) end, desc = "Grep Last Search" },
    { "<leader>sk", "<cmd>Telescope keymaps<cr>", desc = "Keymaps" },
    { "<leader>di", function() Utils("installed_plugins", { type = "custom" }) end, desc = "Installed Plugins" },
    { "<leader>gi", function() Utils("grep_installed_plugins", { type = "custom" }) end, desc = "Installed Plugins" },
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
      desc = "Help Tags",
    },
    { "<leader>sd", "<cmd>Telescope diagnostics bufnr=0<cr>", desc = "Buffer Diagnostics" },
    { "<leader>sD", "<cmd>Telescope diagnostics<cr>", desc = "Workspace Diagnostics" },
    { "<leader>sq", "<cmd>Telescope quickfix theme=ivy<cr>", desc = "Quickfix" },
    { "<leader>sl", "<cmd>Telescope loclist theme=ivy<cr>", desc = "Loclist" },
  },
}

local FZFConfig = {}
local Utils = require("utils.telescope")

function FZFConfig.get_config()
  return {
    "ibhagwan/fzf-lua",
    cmd = "FzfLua",
    dependencies = "nvim-tree/nvim-web-devicons",
    config = function()
      require("fzf-lua").setup({
        keymap = {
          fzf = {
            ["ctrl-q"] = "select-all+accept",
          },
        },
        -- winopts = {
        --   height = 0.5,
        --   width = 0.7,
        --   row = 0.5,
        --   hl = { normal = "Pmenu" },
        --   border = "none",
        -- },
        fzf_opts = {
          ["--no-info"] = "",
          ["--info"] = "hidden",
          ["--padding"] = "13%,5%,13%,5%",
          ["--header"] = " ",
          ["--no-scrollbar"] = "",
        },
        files = {
          formatter = "path.filename_first",
          git_icons = true,
          prompt = "files:",
          preview_opts = "hidden",
          -- no_header = true,
          -- cwd_header = false,
          -- cwd_prompt = false,
        },
        buffers = {
          formatter = "path.filename_first",
          prompt = "buffers:",
          preview_opts = "hidden",
          no_header = true,
          fzf_opts = { ["--delimiter"] = " ", ["--with-nth"] = "-1.." },
        },
        helptags = {
          prompt = "💡:",
          preview_opts = "hidden",
          winopts = {
            row = 1,
            width = vim.api.nvim_win_get_width(0),
            height = 0.3,
          },
        },
        git = {
          bcommits = {
            prompt = "logs:",
            cmd = "git log --color --pretty=format:'%C(yellow)%h%Creset %Cgreen%><(12)%cr%><|(12)%Creset %s' <file>",
            preview = "git show --stat --color --format='%C(cyan)%an%C(reset)%C(bold yellow)%d%C(reset): %s' {1} -- <file>",
            actions = {
              ["ctrl-d"] = function(...)
                fzf.actions.git_buf_vsplit(...)
                vim.cmd("windo diffthis")
                local switch = vim.api.nvim_replace_termcodes("<C-w>h", true, false, true)
                vim.api.nvim_feedkeys(switch, "t", false)
              end,
            },
            preview_opts = "nohidden",
            winopts = {
              preview = {
                layout = "vertical",
                vertical = "right:50%",
                wrap = "wrap",
              },
              row = 1,
              width = vim.api.nvim_win_get_width(0),
              height = 0.3,
            },
          },
          branches = {
            prompt = "branches:",
            cmd = "git branch --all --color",
            winopts = {
              preview = {
                layout = "vertical",
                vertical = "right:50%",
                wrap = "wrap",
              },
              row = 1,
              width = vim.api.nvim_win_get_width(0),
              height = 0.3,
            },
          },
        },
        autocmds = {
          prompt = "autocommands:",
          winopts = {
            width = 0.8,
            height = 0.7,
            preview = {
              layout = "horizontal",
              horizontal = "down:40%",
              wrap = "wrap",
            },
          },
        },
        keymaps = {
          prompt = "keymaps:",
          winopts = {
            width = 0.8,
            height = 0.7,
          },
          actions = {
            ["default"] = function(selected)
              local lines = vim.split(selected[1], "│", {})
              local mode, key = lines[1]:gsub("%s+", ""), lines[2]:gsub("%s+", "")
              vim.cmd("verbose " .. mode .. "map " .. key)
            end,
          },
        },
        highlights = {
          prompt = "highlights:",
          winopts = {
            width = 0.8,
            height = 0.7,
            preview = {
              layout = "horizontal",
              horizontal = "down:40%",
              wrap = "wrap",
            },
          },
          actions = {
            ["default"] = function(selected)
              print(vim.cmd.highlight(selected[1]))
            end,
          },
        },
        lsp = {
          code_actions = {
            prompt = "code actions:",
            winopts = {
              width = 0.8,
              height = 0.7,
              preview = {
                layout = "horizontal",
                horizontal = "up:75%",
              },
            },
          },
        },
        registers = {
          prompt = "registers:",
          preview_opts = "hidden",
          winopts = {
            width = 0.8,
            height = 0.7,
            preview = {
              layout = "horizontal",
              horizontal = "down:45%",
            },
          },
        },
      })

      require("fzf-lua").register_ui_select()
    end,

  -- stylua: ignore
  keys = {
    { "<leader><leader>", "<cmd>FzfLua buffers<cr>", desc = "Buffers" },
    { "<leader>/", "<cmd>FzfLua blines<cr>", desc = "Curent Buffer Fuzzy Find" },
    { "<leader>gr", "<cmd>FzfLua grep_cword theme=ivy<cr>", mode = { "n", "v" }, desc = "Grep String" },
    { "<leader>s:", "<cmd>FzfLua search_history<cr>", desc = "Search History" },
    { "<leader>sH", "<cmd>FzfLua highlights<cr>", desc = "Search Highlights" },
    { "<leader>sc", "<cmd>FzfLua command_history<cr>", desc = "Command History" },
    { "<leader>.", "<cmd>FzfLua resume<cr>", desc = "FzfLua Resume" },
    { "<leader>ff", "<cmd>FzfLua files<cr>", desc = "Project files" },
    { "<leader>fR", function() require("fzf-lua").files({ cwd = vim.fn.expand('%:p:h') }) end, desc = "Find Related Files" },
    { "<leader>fo", "<cmd>FzfLua oldfiles", desc = "Old files" },
    -- TODO: implement
    { "<leader>fF", function() require("fzf-lua").files() end, desc = "Find All Files" },
    -- TODO: implement
    { "<leader>sF", function() require("fzf-lua").grep() end, desc = "Search All Files" },
    { "<leader>sS", "<cmd>FzfLua lsp_live_workspace_symbols<cr>", desc = "Workspace Symbols" },
    { "<leader>ss", "<cmd>FzfLua lsp_document_symbols<cr>", desc = "Document symbols" },
    { "<leader>gR", "<cmd>FzfLua lsp_references<cr>", desc = "LSP References" },
    { "<leader>gD", "<cmd>FzfLua lsp_definitions<cr>", desc = "LSP Definitions" },
    { "<leader>I", "<cmd>FzfLua lsp_incoming_calls<cr>", desc = "Incomming calls" },
    { "<leader>O", "<cmd>FzfLua lsp_outgoing_calls<cr>", desc = "Outgiong calls" },
    { "<leader>gg", "<cmd>FzfLua live_grep<cr>", desc = "Live Grep" },
    { "<leader>rg", "<cmd>FzfLua live_grep<cr>", desc = "FzfLua: multi [r]ip[g]rep" },
    { "<leader>g/", "<cmd>FzfLua grep_last<cr>", desc = "Grep Last Search" },
    { "<leader>sk", "<cmd>FzfLua keymaps<cr>", desc = "Keymaps" },
    -- { "<leader>di", function() Utils("installed_plugins", { type = "custom" }) end, desc = "Installed Plugins" },
    -- { "<leader>gi", function() Utils("grep_installed_plugins", { type = "custom" }) end, desc = "Installed Plugins" },
    { "<leader>u", "<cmd>FzfLua changes<cr>", desc = "Undo (Changes)" },
    { "<leader>sj", "<cmd>FzfLua jumps<cr>", desc = "Jumplist" },
    { "<leader>sm", "<cmd>FzfLua marks<cr>", desc = "Marks" },
    { "<leader>sr", "<cmd>FzfLua registers<cr>", desc = "Registers" },
    { "<leader>sh", "<cmd>FzfLua helptags<cr>", desc = "Help Tags" },
    { "<leader>sd", "<cmd>FzfLua diagnostics_document<cr>", desc = "Buffer Diagnostics" },
    { "<leader>sD", "<cmd>FzfLua diagnostics_workspace<cr>", desc = "Workspace Diagnostics" },
    { "<leader>sq", "<cmd>FzfLua quickfix<cr>", desc = "Quickfix" },
    { "<leader>sl", "<cmd>FzfLua loclist<cr>", desc = "Loclist" },
  },
  }
end

if vim.g.use_fzf then
  return FZFConfig.get_config()
else
  -- return basic config
  return {
    "ibhagwan/fzf-lua",
    cmd = "FzfLua",
    dependencies = "nvim-tree/nvim-web-devicons",
  }
end

local FZFConfig = {}

local lower_third = {
  row = 1,
  width = vim.api.nvim_win_get_width(0),
  height = 0.3,
}

local lower_third_vertical_preview = {
  preview = {
    layout = "vertical",
    vertical = "right:50%",
    wrap = "wrap",
  },
  row = 1,
  width = vim.api.nvim_win_get_width(0),
  height = 0.3,
}

function FZFConfig.get_config()
  return {
    "ibhagwan/fzf-lua",
    cmd = "FzfLua",
    config = function()
      require("fzf-lua").setup({
        defaults = {
          file_icons = "mini",
        },
        keymap = {
          fzf = {
            ["ctrl-q"] = "select-all+accept",
          },
        },
        winopts = {
          border = "none",
          preview = {
            layout = "horizontal",
            horizontal = "down:40%",
            wrap = "wrap",
          },
        },
        fzf_opts = {
          ["--no-info"] = "",
          ["--info"] = "hidden",
          ["--padding"] = "3%,3%,3%,3%",
          ["--header"] = " ",
          ["--no-scrollbar"] = "",
        },
        files = {
          formatter = "path.filename_first",
          git_icons = true,
          prompt = "files:",
          winopts = {
            preview = { hidden = "hidden" },
          },
        },
        buffers = {
          formatter = "path.filename_first",
          prompt = "buffers:",
          no_header = true,
          fzf_opts = { ["--delimiter"] = " ", ["--with-nth"] = "-1.." },
          winopts = vim.tbl_extend("force", lower_third, { preview = { hidden = "hidden" } }),
          actions = {
            ["ctrl-d"] = function(selected)
              require("fzf-lua").actions.buf_del(selected)
              require("fzf-lua").buffers()
            end,
          },
        },
        helptags = {
          prompt = "💡:",
          winopts = vim.tbl_extend("force", lower_third, { preview = { hidden = "hidden" } }),
        },
        git = {
          bcommits = {
            prompt = "logs:",
            cmd = "git log --color --pretty=format:'%C(yellow)%h%Creset %Cgreen%><(12)%cr%><|(12)%Creset %s' <file>",
            preview = "git show --stat --color --format='%C(cyan)%an%C(reset)%C(bold yellow)%d%C(reset): %s' {1} -- <file>",
            actions = {
              ["ctrl-d"] = function(...)
                require("fzf-lua").actions.git_buf_vsplit(...)
                vim.cmd("windo diffthis")
                local switch = vim.api.nvim_replace_termcodes("<C-w>h", true, false, true)
                vim.api.nvim_feedkeys(switch, "t", false)
              end,
            },
            winopts = vim.tbl_extend("force", lower_third_vertical_preview, { preview = { hidden = "nohidden" } }),
          },
          branches = {
            prompt = "branches:",
            cmd = "git branch --all --color",
            winopts = lower_third_vertical_preview,
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
            },
          },
          winopts = {
            preview = {
              layout = "horizontal",
              horizontal = "up:75%",
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
        grep = {
          rg_glob = true,
          glob_flag = "--iglob",
          glob_separator = "%s%-%-",
          rg_opts = "--column --line-number --no-heading --color=always --smart-case --hidden --glob=!.git/ --max-columns=4096 -e",
          winopts = {
            preview = {
              layout = "horizontal",
              horizontal = "down:45%",
            },
          },
          actions = {
            ["ctrl-g"] = { require("fzf-lua").actions.grep_lgrep },
            ["ctrl-r"] = { require("fzf-lua").actions.toggle_ignore },
          },
        },
      })

      require("fzf-lua").register_ui_select()
    end,

    -- stylua: ignore
    keys = {
      { "<leader><leader>", "<cmd>FzfLua buffers<cr>", desc = "Buffers" },
      { "<leader>/", "<cmd>FzfLua lgrep_curbuf<cr>", desc = "Curent Buffer Fuzzy Find" },
      { "<leader>gr", "<cmd>FzfLua grep_cword theme=ivy<cr>", mode = { "n"}, desc = "Grep String" },
      { "<leader>gr", "<cmd>FzfLua grep_visual theme=ivy<cr>", mode = { "v"}, desc = "Grep String" },
      { "<leader>s:", "<cmd>FzfLua search_history<cr>", desc = "Search History" },
      { "<leader>sH", "<cmd>FzfLua highlights<cr>", desc = "Search Highlights" },
      { "<leader>sc", "<cmd>FzfLua command_history<cr>", desc = "Command History" },
      { "<leader>.", "<cmd>FzfLua resume<cr>", desc = "FzfLua Resume" },
      { "<leader>ff", "<cmd>FzfLua files<cr>", desc = "Project files" },
      { "<leader>fR", function() require("fzf-lua").files({ cwd = vim.fn.expand('%:p:h') }) end, desc = "Find Related Files" },
      { "<leader>fo", "<cmd>FzfLua oldfiles<cr>", desc = "Old files" },
      { "<leader>fF", function() require("fzf-lua").files({ cmd = "fd --color=never --type f --hidden --follow --exclude .git --no-ignore" }) end, desc = "Find All Files" },
      { "<leader>sF", function() require("fzf-lua").grep({ cmd = "rg --column --line-number --no-heading --color=always --smart-case --max-columns=4096 --no-ignore --hidden" }) end, desc = "Search All Files" },
      { "<leader>gdf", function()require("fzf-lua").grep({raw_cmd = [[git status -su | rg "^\s*M" | cut -d ' ' -f3 | xargs rg --hidden --column --line-number --no-heading --color=always  --with-filename -e '']]}) end, desc = "Grep Git Diff Files"},
      { "<leader>sS", "<cmd>FzfLua lsp_live_workspace_symbols<cr>", desc = "Workspace Symbols" },
      { "<leader>ss", "<cmd>FzfLua lsp_document_symbols<cr>", desc = "Document symbols" },
      { "<leader>gR", "<cmd>FzfLua lsp_references<cr>", desc = "LSP References" },
      { "<leader>gD", "<cmd>FzfLua lsp_definitions<cr>", desc = "LSP Definitions" },
      { "<leader>I", "<cmd>FzfLua lsp_incoming_calls<cr>", desc = "Incomming calls" },
      { "<leader>O", "<cmd>FzfLua lsp_outgoing_calls<cr>", desc = "Outgiong calls" },
      { "<leader>gg", "<cmd>FzfLua live_grep<cr>", desc = "Live Grep" },
      { "<leader>rg", "<cmd>FzfLua live_grep_glob<cr>", desc = "FzfLua: multi [r]ip[g]rep" },
      { "<leader>rG", function() require("fzf-lua").live_grep({ cmd = "rg --column --line-number --no-heading --color=always --smart-case --max-columns=4096 --no-ignore --hidden" }) end, desc = "FzfLua: live grep in all files" },
      { "<leader>g/", "<cmd>FzfLua grep_last<cr>", desc = "Grep Last Search" },
      { "<leader>sk", "<cmd>FzfLua keymaps<cr>", desc = "Keymaps" },
      -- { "<leader>di", function() Utils("installed_plugins", { type = "custom" }) end, desc = "Installed Plugins" },
      -- { "<leader>gi", function() Utils("grep_installed_plugins", { type = "custom" }) end, desc = "Installed Plugins" },
      { "<leader>U", "<cmd>FzfLua changes<cr>", desc = "Undo (Changes)" },
      { "<leader>sj", "<cmd>FzfLua jumps<cr>", desc = "Jumplist" },
      { "<leader>sm", "<cmd>FzfLua marks<cr>", desc = "Marks" },
      { "<leader>sr", "<cmd>FzfLua registers<cr>", desc = "Registers" },
      { "<leader>sh", "<cmd>FzfLua helptags<cr>", desc = "Help Tags" },
      { "<leader>sd", "<cmd>FzfLua diagnostics_document<cr>", desc = "Buffer Diagnostics" },
      { "<leader>sD", "<cmd>FzfLua diagnostics_workspace<cr>", desc = "Workspace Diagnostics" },
      { "<leader>sq", "<cmd>FzfLua quickfix<cr>", desc = "Quickfix" },
      { "<leader>sl", "<cmd>FzfLua loclist<cr>", desc = "Loclist" },
      { "<leader>si", "<cmd>FzfLua treesitter<cr>", desc = "Treesitter" },
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
  }
end

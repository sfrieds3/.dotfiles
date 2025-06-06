local M = {
  "stevearc/overseer.nvim",
  cmd = {
    "Grep",
    "GrepAll",
    "GrepCWord",
    "GrepVWord",
    "Make",
    "OverseerDebugParser",
    "OverseerInfo",
    "OverseerOpen",
    "OverseerRun",
    "OverseerRunCmd",
    "OverseerToggle",
    "OverseerQuickAction",
    "OverseerTaskAction",
  },
  keys = {
    "<localleader>g",
    "<localleader>G",
    { "<localleader>G", mode = { "x" } },
    { "<localleader>.", "<cmd>OverseerQuickAction restart<cr>", desc = "Overseer restart" },
  },
}

local overseer_grep_cmd_opts = { nargs = "*", bang = true, complete = "file" }

local function overseer_grep_components(params)
  return {
    {
      "on_output_quickfix",
      errorformat = vim.o.grepformat,
      open = not params.bang,
      open_height = 8,
      items_only = true,
    },
    -- We don't care to keep this around as long as most tasks
    { "on_complete_dispose", timeout = 30 },
    "default",
  }
end

local function build_grep_cmd(params, opts)
  params = opts.params or params
  local rg_flags = opts.rg_flags or ""

  -- Insert args at the '$*' in the grepprg
  local cmd, num_subs = vim.o.grepprg:gsub("%$%*", params.args)
  if num_subs == 0 then
    cmd = cmd .. " " .. params.args
  end

  if rg_flags ~= nil then
    cmd = cmd .. " " .. rg_flags
  end

  return vim.fn.expandcmd(cmd)
end

function M.config()
  local overseer = require("overseer")
  overseer.setup({})

  local grep_cmds = {
    Grep = {},
    GrepAll = { rg_flags = "-uuu --hidden" },
    GrepHidden = { rg_flags = "--hidden" },
    GrepCWord = {
      params = {
        args = function()
          return vim.fn.expand("<cword>")
        end,
      },
    },
    GrepVWord = {
      params = {
        args = function()
          return require("utils.utils").get_visual_selection()
        end,
      },
    },
  }

  for cmd, opts in pairs(grep_cmds) do
    vim.api.nvim_create_user_command(cmd, function(params)
      local task = overseer.new_task({
        cmd = build_grep_cmd(params, opts),
        components = overseer_grep_components(params),
      })
      task:start()
    end, overseer_grep_cmd_opts)
  end

  vim.keymap.set("n", "<localleader>g", ":Grep<space>")
  vim.keymap.set("n", "<localleader>G", ":GrepCWord<cr>")
  vim.keymap.set("x", "<localleader>G", ":<C-u>GrepVWord<cr>")

  vim.api.nvim_create_user_command("Make", function(params)
    -- Insert args at the '$*' in the makeprg
    local cmd, num_subs = vim.o.grepprg:gsub("%$%*", vim.fn.shellescape(params.args))
    if num_subs == 0 then
      cmd = cmd .. " " .. params.args
    end
    local task = require("overseer").new_task({
      cmd = vim.fn.expandcmd(cmd),
      components = {
        { "on_output_quickfix", open = not params.bang, open_height = 8 },
        "unique",
        "default",
      },
    })
    task:start()
  end, {
    desc = "Run your makeprg as an Overseer task",
    nargs = "*",
    bang = true,
  })
end

return M

local M = {
  "stevearc/overseer.nvim",
  cmd = {
    "Grep",
    "Grepcword",
    "Grepvword",
    "Make",
    "OverseerDebugParser",
    "OverseerInfo",
    "OverseerOpen",
    "OverseerRun",
    "OverseerRunCmd",
    "OverseerToggle",
  },
  keys = {
    "<localleader>g",
    "<localleader>G",
    { "<localleader>G", mode = { "x" } },
  },
}

function M.config()
  local overseer = require("overseer")
  overseer.setup({})
  vim.api.nvim_create_user_command("Grep", function(params)
    -- Insert args at the '$*' in the grepprg
    local cmd, num_subs = vim.o.grepprg:gsub("%$%*", params.args)
    if num_subs == 0 then
      cmd = cmd .. " " .. params.args
    end
    local task = overseer.new_task({
      cmd = vim.fn.expandcmd(cmd),
      components = {
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
      },
    })
    task:start()
  end, { nargs = "*", bang = true, complete = "file" })
  vim.keymap.set("n", "<localleader>g", ":Grep<space>")

  vim.api.nvim_create_user_command("Grepcword", function()
    local params = {}
    params.args = vim.fn.expand("<cword>")
    -- Insert args at the '$*' in the grepprg
    local cmd, num_subs = vim.o.grepprg:gsub("%$%*", params.args)
    if num_subs == 0 then
      cmd = cmd .. " " .. params.args
    end
    local task = overseer.new_task({
      cmd = vim.fn.expandcmd(cmd),
      components = {
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
      },
    })
    task:start()
  end, { nargs = "*", bang = true, complete = "file" })
  vim.keymap.set("n", "<localleader>G", ":Grepcword<cr>")

  vim.api.nvim_create_user_command("Grepvword", function()
    local params = {}
    params.args = require("utils.utils").get_visual_selection(0)
    -- Insert args at the '$*' in the grepprg
    local cmd, num_subs = vim.o.grepprg:gsub("%$%*", params.args)
    if num_subs == 0 then
      cmd = cmd .. " " .. params.args
    end
    local task = overseer.new_task({
      cmd = vim.fn.expandcmd(cmd),
      components = {
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
      },
    })
    task:start()
  end, { nargs = "*", bang = true, complete = "file" })
  vim.keymap.set("x", "<localleader>G", ":<C-u>Grepvword<cr>")

  vim.api.nvim_create_user_command("Make", function(params)
    -- Insert args at the '$*' in the makeprg
    local cmd, num_subs = vim.o.makeprg:gsub("%$%*", params.args)
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

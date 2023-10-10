local M = {}

function M.setup()
  local dap = require("dap")
  local widgets = require("dap.ui.widgets")

  dap.defaults.fallback.terminal_win_cmd = "50split new"

  vim.keymap.set({ "n", "t" }, "<F3>", dap.terminate, { desc = "dap: terminate" })
  vim.keymap.set({ "n", "t" }, "<F5>", dap.continue, { desc = "dap: start/continue debug session" })
  vim.keymap.set("n", "<F10>", dap.step_over, { desc = "dap: step_over" })
  vim.keymap.set("n", "<F11>", dap.step_into, { desc = "dap: step_into" })
  vim.keymap.set("n", "<F12>", dap.step_out, { desc = "dap: step_out" })
  vim.keymap.set("n", "\\b", dap.toggle_breakpoint, { desc = "[d]ap: toggle [b]reakpoint" })
  vim.keymap.set("n", "\\B", function()
    dap.toggle_breakpoint(vim.fn.input("Breakpoint Condition: "), nil, nil, true)
  end, { desc = "[d]ap: set conditional [B]reakpoint" })
  vim.keymap.set("n", "\\lp", function()
    dap.toggle_breakpoint(nil, nil, vim.fn.input("Log point message: "), true)
  end, { desc = "dap: set [l]og [p]oint message breapoint" })
  vim.keymap.set("n", "\\dr", dap.repl.toggle, { desc = "[d]ap: toggle [r]epl" })
  vim.keymap.set("n", "\\dl", dap.run_last, { desc = "[d]ap: run [l]ast" })

  vim.keymap.set("n", "\\dR", dap.restart_frame, { desc = "[d]ap: [R]estart_frame" })
  vim.keymap.set("n", "\\dj", dap.down, { desc = "[d]ap down" })
  vim.keymap.set("n", "\\dk", dap.up, { desc = "[d]ap up" })
  vim.keymap.set("n", "\\dc", dap.run_to_cursor, { desc = "[d]ap run to [c]ursor" })
  vim.keymap.set("n", "\\dg", dap.goto_, { desc = "[d]ap [g]oto" })
  vim.keymap.set("n", "\\dS", function()
    widgets.centered_float(widgets.frames)
  end, { desc = "[d]ap show frames" })
  vim.keymap.set("n", "\\dt", function()
    widgets.centered_float(widgets.threads)
  end, { desc = "[d]ap show threads" })
  vim.keymap.set("n", "\\ds", function()
    widgets.centered_float(widgets.scopes)
  end, { desc = "[d]ap show scopes" })
  vim.keymap.set({ "n", "v" }, "\\dh", widgets.hover, { desc = "[d]ap [h]over" })
  vim.keymap.set({ "n", "v" }, "\\dp", widgets.preview, { desc = "[d]ap [p]review" })

  dap.listeners.after.event_initialized["me.dap"] = function()
    vim.keymap.set("n", "<down>", dap.step_over, { desc = "dap: step over" })
    vim.keymap.set("n", "<left>", dap.step_out, { desc = "dap: step out" })
    vim.keymap.set("n", "<right>", dap.step_into, { desc = "dap: step into" })
    vim.o.signcolumn = "yes:1"
  end
  local after_session = function()
    pcall(vim.keymap.del, "n", "<down>")
    pcall(vim.keymap.del, "n", "<left>")
    pcall(vim.keymap.del, "n", "<right>")
    vim.o.signcolumn = "auto"
  end
  dap.listeners.after.event_terminated["me.dap"] = after_session
  dap.listeners.after.disconnected["me.dap"] = after_session

  local sidebar = widgets.sidebar(widgets.scopes)
  vim.api.nvim_create_user_command("DapSidebar", sidebar.toggle, { nargs = 0 })
  vim.api.nvim_create_user_command("DapBreakpoints", function()
    dap.list_breakpoints(true)
  end, { nargs = 0 })
end

return M

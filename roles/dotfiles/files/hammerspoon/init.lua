require("reload")
local focus = require("focus")

local browser = "firefox"
local terminal = "alacritty"
local editor = "Emacs"

hs.hotkey.bind({ "cmd", "shift" }, "j", function()
  focusandback(editor)
end)
hs.hotkey.bind({ "cmd", "shift" }, "k", function()
  focusandback(browser)
end)
hs.hotkey.bind({ "cmd", "shift" }, "h", function()
  focusandback(terminal)
end)
hs.hotkey.bind({ "cmd", "shift" }, "n", function()
  focusandback("insomnia")
end)

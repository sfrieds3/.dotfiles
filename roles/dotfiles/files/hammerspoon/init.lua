require("reload")
local focus = require("focus")

local browser = "firefox"
local terminal = "alacritty"

hs.hotkey.bind({ "cmd", "shift" }, "k", function()
  focusandback(browser)
end)
hs.hotkey.bind({ "cmd", "shift" }, "j", function()
  focusandback(terminal)
end)
hs.hotkey.bind({ "cmd", "shift" }, "n", function()
  focusandback("insomnia")
end)

require("reload")
local focus = require("focus")
local switchscreen = require("switchscreen")

--
hs.hotkey.bind({ "ctrl", "alt", "cmd" }, "f3", function()
  hs.console.alpha(0.9)
  hs.toggleConsole()
end)

-- focus windows
local browser = "firefox"
local terminal = "wezterm"
local api_query = "Bruno"
local passwords = "1Password"

hs.hotkey.bind({ "cmd", "shift" }, "a", function()
  focusandback("Safari")
end)
hs.hotkey.bind({ "cmd", "shift" }, "d", function()
  focusandback("DataGrip")
end)
hs.hotkey.bind({ "cmd", "shift" }, "e", function()
  focusandback("Microsoft Outlook")
end)
hs.hotkey.bind({ "cmd", "shift" }, "f", function()
  focusandback(browser)
end)
hs.hotkey.bind({ "cmd", "shift" }, "g", function()
  focusandback("Google Chrome")
end)
hs.hotkey.bind({ "cmd", "shift" }, "k", function()
  focusandback("PyCharm Professional Edition")
end)
hs.hotkey.bind({ "cmd", "shift" }, "l", function()
  focusandback("Calendar")
end)
hs.hotkey.bind({ "cmd", "shift" }, "m", function()
  focusandback("Mattermost")
end)
hs.hotkey.bind({ "cmd", "shift" }, "q", function()
  focusandback(api_query)
end)
hs.hotkey.bind({ "cmd", "shift" }, "s", function()
  focusandback("Spotify")
end)
hs.hotkey.bind({ "cmd", "shift" }, ",", function()
  focusandback(terminal)
end)
hs.hotkey.bind({ "cmd", "shift" }, ";", function()
  focusandback("Reminders")
end)
hs.hotkey.bind({ "cmd", "shift" }, "'", function()
  focusandback(passwords)
end)
hs.hotkey.bind({ "cmd", "shift" }, "`", function()
  focusandback("anybox")
end)

-- switch screens quickly
hs.hotkey.bind({ "ctrl", "cmd" }, "]", function()
  switchscreen.focusScreen(hs.mouse.getCurrentScreen():next())
end)
hs.hotkey.bind({ "ctrl", "cmd" }, "[", function()
  switchscreen.focusScreen(hs.mouse.getCurrentScreen():previous())
end)

-- move window to screen
local function windowToNextScreen()
  local app = hs.window.focusedWindow()
  app:moveToScreen(app:screen():next())
end
local function windowToPreviousScreen()
  local app = hs.window.focusedWindow()
  app:moveToScreen(app:screen():previous())
end

hs.hotkey.bind({ "ctrl", "cmd", "shift" }, "]", function()
  windowToNextScreen()
end)
hs.hotkey.bind({ "ctrl", "cmd", "shift" }, "[", function()
  windowToPreviousScreen()
end)

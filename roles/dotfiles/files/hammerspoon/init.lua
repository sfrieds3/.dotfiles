require("reload")
local focus = require("focus")
local switchscreen = require("switchscreen")

hs.alert.defaultStyle["atScreenEdge"] = 2

hs.hotkey.bind({ "ctrl", "alt", "cmd" }, "f3", function()
  hs.console.alpha(0.9)
  hs.toggleConsole()
end)

-- Double Cmd+Q twice to quit
local quitModal = hs.hotkey.modal.new("cmd", "q")

function quitModal:entered()
  hs.alert.show("Press Cmd+Q again to quit", 1)
  hs.timer.doAfter(1, function()
    quitModal:exit()
  end)
end

local function doQuit()
  local app = hs.application.frontmostApplication()
  app:kill()
end

quitModal:bind("cmd", "q", doQuit)
quitModal:bind("", "escape", function()
  quitModal:exit()
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
  focusandback("PyCharm Professional Edition")
end)
hs.hotkey.bind({ "cmd", "shift" }, "f", function()
  focusandback(browser)
end)
hs.hotkey.bind({ "cmd", "shift" }, "g", function()
  focusandback("Google Chrome")
end)
hs.hotkey.bind({ "cmd", "shift" }, "\\", function()
  focusandback("Microsoft Outlook")
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
hs.hotkey.bind({ "cmd", "shift" }, "y", function()
  focusandback("Pocket Casts")
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

-- spotify and volume
hs.hotkey.bind({ "cmd", "shift" }, "0", function()
  hs.spotify.displayCurrentTrack()
end)
hs.hotkey.bind({ "cmd", "shift" }, "p", function()
  hs.spotify.playpause()
end)
hs.hotkey.bind({ "cmd", "shift" }, "j", function()
  hs.spotify.next()
end)
hs.hotkey.bind({ "cmd", "shift" }, "k", function()
  hs.spotify.previous()
end)

-- Volume control
hs.hotkey.bind({ "cmd", "shift" }, "=", function()
  local audio_output = hs.audiodevice.defaultOutputDevice()
  if audio_output:muted() then
    audio_output:setMuted(false)
  end
  audio_output:setVolume(hs.audiodevice.current().volume + 5)
  hs.alert.closeAll()
  hs.alert.show("Volume level: " .. tostring(math.floor(hs.audiodevice.current().volume)) .. "%")
end)
hs.hotkey.bind({ "cmd", "shift" }, "-", function()
  local audio_output = hs.audiodevice.defaultOutputDevice()
  audio_output:setVolume(hs.audiodevice.current().volume - 5)
  hs.alert.closeAll()
  hs.alert.show("Volume level: " .. tostring(math.floor(hs.audiodevice.current().volume)) .. "%")
end)
hs.hotkey.bind({ "cmd", "shift" }, "u", function()
  local audio_output = hs.audiodevice.defaultOutputDevice()
  if audio_output:muted() then
    audio_output:setMuted(false)
  else
    audio_output:setMuted(true)
  end
end)
hs.hotkey.bind({ "cmd", "shift" }, "w", function()
  local audio_output = hs.audiodevice.defaultOutputDevice()
  hs.alert.closeAll()
  hs.alert.show("Volume level: " .. tostring(math.floor(hs.audiodevice.current().volume)) .. "%")
end)

hs.hotkey.bind({ "cmd", "shift" }, "h", function()
  hs.alert.show([[
  Key bindings:
    0 -> Current Track
    a -> Safari
    d -> DataGrip
    e -> PyCharm
    f -> Browser
    g -> Google Chrome
    j -> Next Song
    k -> Previous Song
    l -> Calendar
    m -> Messaging
    p -> Play/Pause
    q -> Query
    s -> Spotify
    u -> Mute Output
    w -> Volume Level
    y -> Pocket Casts
    \ -> Microsoft Outlook
    , -> Terminal
    ; -> Reminders
    ' -> Passwords
    ` -> Anybox
    - -> Decrease Volume
    + -> Increase Volume
  ]])
end)

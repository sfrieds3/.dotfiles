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
local api_query = "Insomnia"
local passwords = "1Password"
local music_app = "Spotify"

hs.hotkey.bind({ "cmd", "shift" }, "7", function()
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
hs.hotkey.bind({ "cmd", "shift" }, "F1", function()
  focusandback("Microsoft Outlook")
end)
hs.hotkey.bind({ "cmd", "shift" }, "F2", function()
  focusandback("Calendar")
end)
hs.hotkey.bind({ "cmd", "shift" }, "m", function()
  focusandback("Mattermost")
end)
hs.hotkey.bind({ "cmd", "shift" }, "q", function()
  focusandback(api_query)
end)
hs.hotkey.bind({ "cmd", "shift" }, "0", function()
  focusandback(music_app)
end)
hs.hotkey.bind({ "cmd", "shift" }, "F3", function()
  focusandback("Pocket Casts")
end)
hs.hotkey.bind({ "cmd", "shift" }, ",", function()
  focusandback(terminal)
end)
hs.hotkey.bind({ "cmd", "shift" }, "F9", function()
  focusandback("Reminders")
end)
hs.hotkey.bind({ "cmd", "shift" }, "F8", function()
  focusandback(passwords)
end)
hs.hotkey.bind({ "cmd", "shift" }, "F7", function()
  focusandback("anybox")
end)
hs.hotkey.bind({ "cmd", "shift" }, "F6", function()
  focusandback("ChatGPT")
end)

-- switch screens quickly
hs.hotkey.bind({ "ctrl", "cmd" }, "]", function()
  switchscreen.focusScreen(hs.mouse.getCurrentScreen():next())
end)
hs.hotkey.bind({ "ctrl", "cmd" }, "[", function()
  switchscreen.fFromocusScreen(hs.mouse.getCurrentScreen():previous())
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

-- music and volume
local hs_music_app = function()
  if music_app == "Music" then
    return "itunes"
  elseif music_app == "Spotify" then
    return "spotify"
  end
end

hs.hotkey.bind({ "cmd", "shift" }, "9", function()
  hs[hs_music_app()].displayCurrentTrack()
end)
hs.hotkey.bind({ "cmd", "shift" }, "p", function()
  hs[hs_music_app()].playpause()
end)
hs.hotkey.bind({ "cmd", "shift" }, "j", function()
  hs[hs_music_app()].next()
end)
hs.hotkey.bind({ "cmd", "shift" }, "k", function()
  hs[hs_music_app()].previous()
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
    0 -> Music
    9 -> Current Song
    a -> Safari
    d -> DataGrip
    e -> PyCharm
    f -> Browser
    j -> Next Song
    k -> Previous Song
    l -> Calendar
    m -> Messaging
    p -> Play/Pause
    q -> Query
    u -> Mute Output
    w -> Volume Level
    , -> Terminal
    - -> Decrease Volume
    + -> Increase Volume
    F1 -> Microsoft Outlook
    F2 -> Calendar
    F3 -> Pocket Casts
    F6 -> ChatGPT
    F7 -> Anybox
    F8 -> Passwords
    F9 -> Reminders
  ]])
end)

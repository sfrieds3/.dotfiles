local function reload_config(files)
  do_reload = false
  for _, file in pairs(files) do
    if file:sub(-4) == ".lua" then
      do_reload = true
    end
  end
  if do_reload then
    hs.reload()
  end
end
myWatcher = hs.pathwatcher.new(os.getenv("HOME") .. "/.hammerspoon/", reload_config):start()
hs.alert.show("Config loaded")

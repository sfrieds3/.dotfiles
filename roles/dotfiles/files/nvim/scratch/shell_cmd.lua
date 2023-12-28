local on_exit = function(obj)
  P(obj.stdout:gsub("\n", ""))
end

vim.system({ "which", "python3" }, { text = true }, on_exit):wait()

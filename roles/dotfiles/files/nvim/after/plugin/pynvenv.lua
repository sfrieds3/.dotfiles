if not pcall(require, 'pynvenv') then
  return
end

require('pynvenv').setup({
  default_venv = '$HOME/.venv/venv',
  -- auto_venv = true
})

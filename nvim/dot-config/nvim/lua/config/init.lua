vim.g.use_fzf = true

require("utils")
require("config.disable_builtins")
require("config.base")
require("config.autocmd")
require("config.lazy").init_lazy()
require("sfrieds3")
require("config.colorscheme").set_colorscheme()

vim.cmd.packadd("nvim.undotree")
vim.cmd.packadd("nvim.difftool")
require("vim._extui").enable({})

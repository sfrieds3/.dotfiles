vim.g.use_fzf = true

require("utils")
require("config.disable_builtins")
require("config.base")
require("config.autocmd")
require("config.lazy").init_lazy()
require("config.colorscheme").set_colorscheme()
-- require("config.statuscol")

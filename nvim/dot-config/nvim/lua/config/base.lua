-- local vim_config_path = string.format("%s/.vim", os.getenv("HOME"))
-- local data_dir = vim.fn.stdpath("data")
-- local config_path = vim.fn.stdpath("config")

-- https://github.com/neovim/neovim/commit/a389dc2
local termfeatures = vim.g.termfeatures or {}
termfeatures.osc52 = false
vim.g.termfeatures = termfeatures

vim.g.python3_host_prog = "$PYTHON3_VENV"

-- space as leader, \ as localleader
vim.g.mapleader = " "
vim.g.maplocalleader = "\\"

local shell = "zsh"
if IS_MAC then
  vim.opt.shell = string.format("/opt/homebrew/bin/%s", shell, "--login")
else
  vim.opt.shell = string.format("/usr/bin/%s", shell, "--login")
end

vim.opt.termguicolors = true
vim.opt.cursorline = true
vim.opt.hidden = true
vim.opt.autoread = true
vim.opt.modeline = false
vim.opt.ignorecase = true
vim.opt.infercase = true
vim.opt.smartcase = true
vim.opt.showmatch = true
vim.opt.splitbelow = true
vim.opt.splitright = true
vim.opt.incsearch = true
vim.opt.hlsearch = true
vim.opt.exrc = true
vim.opt.secure = true
vim.opt.showmode = false

--indentation
vim.opt.shiftround = true
vim.opt.expandtab = true
vim.opt.smarttab = true
vim.opt.tabstop = 8
vim.opt.shiftwidth = 4
vim.opt.softtabstop = 4
vim.opt.scrolloff = 3

vim.opt.laststatus = 3
-- vim.opt.winbar = "%=%m %F"
vim.opt.backspace = { "indent", "eol", "start" }
vim.opt.encoding = "utf8"
vim.opt.fileencoding = "utf8"
vim.opt.showtabline = 1
vim.opt.formatoptions = "qrn1j"
vim.opt.mouse = "a"
vim.opt.redrawtime = 50000
vim.opt.showbreak = "..."
vim.opt.foldenable = true
vim.opt.foldlevel = 99
vim.opt.foldlevelstart = 99
vim.opt.foldmethod = "expr"
vim.opt.foldexpr = "nvim_treesitter#foldexpr()"
vim.opt.foldcolumn = "0"
vim.opt.signcolumn = "auto"
vim.opt.inccommand = "split"
vim.opt.switchbuf:append({ "useopen", "uselast" })
vim.opt.diffopt:append({ "vertical", "algorithm:histogram" })

vim.opt.fillchars = {
  horiz = "━",
  horizup = "┻",
  horizdown = "┳",
  vert = "┃",
  vertleft = "┫",
  vertright = "┣",
  verthoriz = "╋",
}
vim.opt.listchars = { tab = "  ", extends = "›", precedes = "‹", nbsp = "␣", trail = "·", eol = " " }
vim.opt.list = true

vim.opt.timeout = true
vim.opt.timeoutlen = 500
vim.opt.ttimeout = true
vim.opt.ttimeoutlen = 10
vim.opt.updatetime = 500

vim.opt.wildmenu = true
vim.opt.wildignorecase = true
vim.opt.wildignore = { "*.o", "*.pyc", "__pycache__/*", ".venv/*" }
vim.opt.wildcharm = 26 -- <C-z>
vim.opt.tags = "./tags;,tags;"
vim.opt.completeopt = { "menu", "menuone", "preview" }
vim.opt.path = ".,,"
vim.opt.clipboard = "unnamed"

vim.opt.undofile = true
vim.opt.undolevels = 10000
vim.opt.backup = true
vim.opt.backupdir = vim.opt.backupdir - "."
vim.opt.backupext = ".bak"
vim.opt.swapfile = false
vim.opt.shada = [['1000,f1,<1000,/10000,:10000]]

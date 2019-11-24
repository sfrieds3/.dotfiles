"%% SiSU Vim color file
" Pup Maintainer: Scott <s@thisnullptr.com>
" most of these ideas came from Slate

:set background=dark
:highlight clear
if version > 580
 highlight clear
 if exists("syntax_on")
 syntax reset
 endif
endif
let colors_name = "pup"

:highlight Normal guifg=White guibg=black
:highlight Cursor guibg=khaki guifg=slategrey
:highlight VertSplit guibg=#c2bfa5 guifg=black gui=none cterm=reverse
:highlight Folded guibg=black guifg=grey40 ctermfg=grey ctermbg=darkgrey
:highlight FoldColumn guibg=black guifg=grey20 ctermfg=4 ctermbg=7
:highlight IncSearch guifg=green guibg=black cterm=none ctermfg=yellow ctermbg=green
:highlight ModeMsg guifg=goldenrod cterm=none ctermfg=brown
:highlight MoreMsg guifg=SeaGreen ctermfg=darkgreen
:highlight NonText guifg=RoyalBlue guibg=black cterm=bold ctermfg=blue
:highlight Question guifg=springgreen ctermfg=green
:highlight Search guibg=peru guifg=wheat cterm=none ctermfg=grey ctermbg=blue
:highlight SpecialKey guifg=yellowgreen ctermfg=darkgreen
:highlight StatusLine guibg=#c2bfa5 guifg=grey40 gui=none cterm=bold,reverse
:highlight StatusLineNC guibg=#c2bfa5 guifg=black gui=none cterm=reverse
:highlight Title guifg=gold gui=bold cterm=bold ctermfg=yellow
:highlight Statement guifg=CornflowerBlue ctermfg=lightblue
:highlight Visual gui=none guifg=khaki guibg=olivedrab cterm=reverse
:highlight WarningMsg guifg=salmon ctermfg=1
:highlight String guifg=SkyBlue ctermfg=darkcyan
:highlight Comment term=bold ctermfg=11 guifg=grey40
:highlight Constant guifg=#ffa0a0 ctermfg=brown
:highlight Special guifg=darkkhaki ctermfg=brown
:highlight Identifier guifg=salmon ctermfg=red
:highlight Include guifg=red ctermfg=red
:highlight PreProc guifg=red guibg=white ctermfg=red
:highlight Operator guifg=Red ctermfg=Red
:highlight Define guifg=gold gui=bold ctermfg=yellow
:highlight Type guifg=CornflowerBlue ctermfg=2
:highlight Function guifg=navajowhite ctermfg=brown
:highlight Structure guifg=green ctermfg=green
:highlight LineNr guifg=grey50 ctermfg=3
:highlight Ignore guifg=grey40 cterm=bold ctermfg=7
:highlight Todo guifg=orangered guibg=yellow2
:highlight Directory ctermfg=darkcyan
:highlight ErrorMsg cterm=bold guifg=White guibg=Red cterm=bold ctermfg=7 ctermbg=1
:highlight WildMenu ctermfg=0 ctermbg=3
:highlight DiffAdd ctermbg=4
:highlight DiffChange ctermbg=5
:highlight DiffDelete cterm=bold ctermfg=4 ctermbg=6
:highlight DiffText cterm=bold ctermbg=1
:highlight Underlined cterm=underline ctermfg=5
:highlight Error guifg=White guibg=Red cterm=bold ctermfg=7 ctermbg=1
:highlight SpellErrors guifg=White guibg=Red cterm=bold ctermfg=7 ctermbg=1

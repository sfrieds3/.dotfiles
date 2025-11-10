local M = {}

function M.setup(colors)
  local hl = {}

  hl.Normal = { fg = colors.fg, bg = colors.bg }
  hl.NormalFloat = { fg = colors.fg, bg = colors.bg_float }
  hl.FloatBorder = { fg = colors.border, bg = colors.bg_float }
  hl.FloatTitle = { fg = colors.bright_blue, bg = colors.bg_float }

  hl.Cursor = { fg = colors.cursor_fg, bg = colors.cursor_bg }
  hl.lCursor = { fg = colors.cursor_fg, bg = colors.cursor_bg }
  hl.CursorLine = { bg = colors.bg_highlight }
  hl.CursorColumn = { bg = colors.bg_highlight }
  hl.CursorLineNr = { fg = colors.bright_yellow, bold = true }

  hl.LineNr = { fg = colors.fg_gutter }
  hl.SignColumn = { fg = colors.fg_gutter, bg = colors.none }
  hl.Folded = { fg = colors.blue, bg = colors.bg_highlight }
  hl.FoldColumn = { fg = colors.comment, bg = colors.none }

  hl.Visual = { bg = colors.bg_visual }
  hl.VisualNOS = { bg = colors.bg_visual }
  hl.Search = { fg = colors.bg, bg = colors.yellow }
  hl.IncSearch = { fg = colors.bg, bg = colors.bright_yellow }
  hl.CurSearch = { fg = colors.bg, bg = colors.bright_yellow }
  hl.Substitute = { fg = colors.bg, bg = colors.green }

  hl.Pmenu = { fg = colors.fg, bg = colors.bg_popup }
  hl.PmenuSel = { fg = colors.bg, bg = colors.blue }
  hl.PmenuSbar = { bg = colors.bg_popup }
  hl.PmenuThumb = { bg = colors.fg_gutter }

  hl.TabLine = { fg = colors.comment, bg = colors.bg_dark }
  hl.TabLineSel = { fg = colors.bright_white, bg = colors.bg_highlight }
  hl.TabLineFill = { bg = colors.bg_dark }

  hl.StatusLine = { fg = colors.fg_sidebar, bg = colors.bg_statusline }
  hl.StatusLineNC = { fg = colors.fg_gutter, bg = colors.bg_statusline }
  hl.WinSeparator = { fg = colors.border, bg = colors.none }

  hl.Directory = { fg = colors.blue }
  hl.Title = { fg = colors.bright_blue, bold = true }
  hl.ErrorMsg = { fg = colors.error }
  hl.WarningMsg = { fg = colors.warning }
  hl.MoreMsg = { fg = colors.green }
  hl.Question = { fg = colors.blue }
  hl.ModeMsg = { fg = colors.fg, bold = true }

  hl.MatchParen = { fg = colors.bright_yellow, bg = colors.bg_highlight, bold = true }
  hl.SpecialKey = { fg = colors.nontext }
  hl.NonText = { fg = colors.nontext }
  hl.Whitespace = { fg = colors.nontext }
  hl.Conceal = { fg = colors.comment }

  hl.Comment = { fg = colors.comment, italic = true }

  hl.Constant = { fg = colors.bright_cyan }
  hl.String = { fg = colors.green }
  hl.Character = { fg = colors.green }
  hl.Number = { fg = colors.bright_cyan }
  hl.Boolean = { fg = colors.bright_cyan, bold = true }
  hl.Float = { fg = colors.bright_cyan }

  hl.Identifier = { fg = colors.magenta }
  hl.Function = { fg = colors.bright_blue }

  hl.Statement = { fg = colors.magenta }
  hl.Conditional = { fg = colors.magenta }
  hl.Repeat = { fg = colors.magenta }
  hl.Label = { fg = colors.magenta }
  hl.Operator = { fg = colors.cyan }
  hl.Keyword = { fg = colors.magenta }
  hl.Exception = { fg = colors.magenta }

  hl.PreProc = { fg = colors.cyan }
  hl.Include = { fg = colors.magenta }
  hl.Define = { fg = colors.magenta }
  hl.Macro = { fg = colors.cyan }
  hl.PreCondit = { fg = colors.cyan }

  hl.Type = { fg = colors.yellow }
  hl.StorageClass = { fg = colors.yellow }
  hl.Structure = { fg = colors.yellow }
  hl.Typedef = { fg = colors.yellow }

  hl.Special = { fg = colors.bright_cyan }
  hl.SpecialChar = { fg = colors.bright_cyan }
  hl.Tag = { fg = colors.blue }
  hl.Delimiter = { fg = colors.fg }
  hl.SpecialComment = { fg = colors.comment, italic = true }
  hl.Debug = { fg = colors.bright_red }

  hl.Underlined = { underline = true }
  hl.Ignore = { fg = colors.comment }
  hl.Error = { fg = colors.error }
  hl.Todo = { fg = colors.bg, bg = colors.warning, bold = true }

  hl.SpellBad = { sp = colors.error, undercurl = true }
  hl.SpellCap = { sp = colors.warning, undercurl = true }
  hl.SpellRare = { sp = colors.hint, undercurl = true }
  hl.SpellLocal = { sp = colors.info, undercurl = true }

  hl.DiagnosticError = { fg = colors.error }
  hl.DiagnosticWarn = { fg = colors.warning }
  hl.DiagnosticInfo = { fg = colors.info }
  hl.DiagnosticHint = { fg = colors.hint }

  hl.DiagnosticUnderlineError = { sp = colors.error, undercurl = true }
  hl.DiagnosticUnderlineWarn = { sp = colors.warning, undercurl = true }
  hl.DiagnosticUnderlineInfo = { sp = colors.info, undercurl = true }
  hl.DiagnosticUnderlineHint = { sp = colors.hint, undercurl = true }

  hl.DiagnosticVirtualTextError = { fg = colors.error, bg = colors.none }
  hl.DiagnosticVirtualTextWarn = { fg = colors.warning, bg = colors.none }
  hl.DiagnosticVirtualTextInfo = { fg = colors.info, bg = colors.none }
  hl.DiagnosticVirtualTextHint = { fg = colors.hint, bg = colors.none }

  hl.DiagnosticSignError = { fg = colors.error, bg = colors.none }
  hl.DiagnosticSignWarn = { fg = colors.warning, bg = colors.none }
  hl.DiagnosticSignInfo = { fg = colors.info, bg = colors.none }
  hl.DiagnosticSignHint = { fg = colors.hint, bg = colors.none }

  hl.DiffAdd = { bg = colors.diff_add }
  hl.DiffDelete = { bg = colors.diff_delete }
  hl.DiffChange = { bg = colors.diff_change }
  hl.DiffText = { bg = colors.diff_text }

  hl.diffAdded = { fg = colors.git_add }
  hl.diffRemoved = { fg = colors.git_delete }
  hl.diffChanged = { fg = colors.git_change }
  hl.diffOldFile = { fg = colors.yellow }
  hl.diffNewFile = { fg = colors.bright_yellow }
  hl.diffFile = { fg = colors.blue }
  hl.diffLine = { fg = colors.comment }
  hl.diffIndexLine = { fg = colors.magenta }

  hl.gitcommitSummary = { fg = colors.bright_blue }
  hl.gitcommitOverflow = { fg = colors.error }
  hl.gitcommitComment = { fg = colors.comment, italic = true }
  hl.gitcommitUntracked = { fg = colors.comment }
  hl.gitcommitDiscarded = { fg = colors.comment }
  hl.gitcommitSelected = { fg = colors.comment }
  hl.gitcommitUnmerged = { fg = colors.git_text }
  hl.gitcommitBranch = { fg = colors.magenta, bold = true }

  hl.TreesitterContext = { bg = colors.bg_highlight }

  hl["@variable"] = { fg = colors.white }
  hl["@variable.builtin"] = { fg = colors.red }
  hl["@variable.parameter"] = { fg = colors.red }
  hl["@variable.member"] = { fg = colors.magenta }

  hl["@constant"] = { fg = colors.cyan }
  hl["@constant.builtin"] = { fg = colors.cyan }
  hl["@constant.macro"] = { fg = colors.cyan }

  hl["@module"] = { fg = colors.cyan }
  hl["@label"] = { fg = colors.blue }

  hl["@string"] = { fg = colors.green }
  hl["@string.documentation"] = { fg = colors.green }
  hl["@string.regexp"] = { fg = colors.bright_cyan }
  hl["@string.escape"] = { fg = colors.bright_cyan }
  hl["@string.special"] = { fg = colors.bright_cyan }

  hl["@character"] = { fg = colors.green }
  hl["@character.special"] = { fg = colors.bright_cyan }

  hl["@boolean"] = { fg = colors.bright_cyan, bold = true }
  hl["@number"] = { fg = colors.bright_cyan }
  hl["@number.float"] = { fg = colors.bright_cyan }

  hl["@type"] = { fg = colors.yellow }
  hl["@type.builtin"] = { fg = colors.yellow }
  hl["@type.definition"] = { fg = colors.yellow }

  hl["@attribute"] = { fg = colors.cyan }
  hl["@property"] = { fg = colors.magenta }

  hl["@function"] = { fg = colors.bright_blue }
  hl["@function.builtin"] = { fg = colors.bright_blue }
  hl["@function.call"] = { fg = colors.bright_blue }
  hl["@function.macro"] = { fg = colors.cyan }
  hl["@function.method"] = { fg = colors.bright_blue }
  hl["@function.method.call"] = { fg = colors.bright_blue }

  hl["@constructor"] = { fg = colors.yellow }

  hl["@keyword"] = { fg = colors.magenta }
  hl["@keyword.coroutine"] = { fg = colors.magenta }
  hl["@keyword.function"] = { fg = colors.magenta }
  hl["@keyword.operator"] = { fg = colors.magenta }
  hl["@keyword.import"] = { fg = colors.magenta }
  hl["@keyword.repeat"] = { fg = colors.magenta }
  hl["@keyword.return"] = { fg = colors.magenta }
  hl["@keyword.exception"] = { fg = colors.magenta }

  hl["@keyword.conditional"] = { fg = colors.magenta }
  hl["@keyword.conditional.ternary"] = { fg = colors.cyan }
  hl["@keyword.directive"] = { fg = colors.magenta }
  hl["@keyword.directive.define"] = { fg = colors.magenta }

  hl["@operator"] = { fg = colors.cyan }

  hl["@punctuation.delimiter"] = { fg = colors.fg }
  hl["@punctuation.bracket"] = { fg = colors.fg }
  hl["@punctuation.special"] = { fg = colors.bright_cyan }

  hl["@comment"] = { link = "Comment" }
  hl["@comment.documentation"] = { fg = colors.comment, italic = true }

  hl["@markup.strong"] = { bold = true }
  hl["@markup.emphasis"] = { italic = true }
  hl["@markup.underline"] = { underline = true }
  hl["@markup.strike"] = { strikethrough = true }
  hl["@markup.heading"] = { fg = colors.bright_blue, bold = true }
  hl["@markup.heading.1"] = { fg = colors.bright_blue, bold = true }
  hl["@markup.heading.2"] = { fg = colors.bright_cyan, bold = true }
  hl["@markup.heading.3"] = { fg = colors.bright_green, bold = true }
  hl["@markup.heading.4"] = { fg = colors.bright_yellow, bold = true }
  hl["@markup.heading.5"] = { fg = colors.bright_magenta, bold = true }
  hl["@markup.heading.6"] = { fg = colors.bright_red, bold = true }
  hl["@markup.quote"] = { fg = colors.comment, italic = true }
  hl["@markup.math"] = { fg = colors.blue }
  hl["@markup.link"] = { fg = colors.cyan, underline = true }
  hl["@markup.link.label"] = { fg = colors.blue }
  hl["@markup.link.url"] = { fg = colors.cyan, underline = true }
  hl["@markup.raw"] = { fg = colors.green }
  hl["@markup.raw.block"] = { fg = colors.fg }
  hl["@markup.list"] = { fg = colors.blue }
  hl["@markup.list.checked"] = { fg = colors.green }
  hl["@markup.list.unchecked"] = { fg = colors.comment }

  hl["@diff.plus"] = { link = "DiffAdd" }
  hl["@diff.minus"] = { link = "DiffDelete" }
  hl["@diff.delta"] = { link = "DiffChange" }

  hl["@tag"] = { fg = colors.blue }
  hl["@tag.attribute"] = { fg = colors.red }
  hl["@tag.delimiter"] = { fg = colors.fg }

  hl.TelescopeBorder = { fg = colors.border, bg = colors.bg_float }
  hl.TelescopeNormal = { fg = colors.fg, bg = colors.bg_float }
  hl.TelescopePromptBorder = { fg = colors.border, bg = colors.bg_float }
  hl.TelescopePromptNormal = { fg = colors.fg, bg = colors.bg_float }
  hl.TelescopePromptPrefix = { fg = colors.blue }
  hl.TelescopeSelection = { fg = colors.fg, bg = colors.bg_visual }
  hl.TelescopeSelectionCaret = { fg = colors.blue, bg = colors.bg_visual }
  hl.TelescopeTitle = { fg = colors.bright_blue, bold = true }

  hl.NeoTreeNormal = { fg = colors.fg_sidebar, bg = colors.bg_sidebar }
  hl.NeoTreeNormalNC = { fg = colors.fg_sidebar, bg = colors.bg_sidebar }
  hl.NeoTreeRootName = { fg = colors.bright_blue, bold = true }
  hl.NeoTreeGitAdded = { fg = colors.git_add }
  hl.NeoTreeGitDeleted = { fg = colors.git_delete }
  hl.NeoTreeGitModified = { fg = colors.git_change }
  hl.NeoTreeGitConflict = { fg = colors.error, bold = true }
  hl.NeoTreeGitUntracked = { fg = colors.comment }
  hl.NeoTreeIndentMarker = { fg = colors.fg_gutter }
  hl.NeoTreeSymbolicLinkTarget = { fg = colors.cyan }

  hl.WhichKey = { fg = colors.bright_blue }
  hl.WhichKeyGroup = { fg = colors.cyan }
  hl.WhichKeyDesc = { fg = colors.fg }
  hl.WhichKeySeperator = { fg = colors.comment }
  hl.WhichKeyFloat = { bg = colors.bg_float }
  hl.WhichKeyBorder = { fg = colors.border, bg = colors.bg_float }

  hl.GitSignsAdd = { fg = colors.git_add }
  hl.GitSignsChange = { fg = colors.git_change }
  hl.GitSignsDelete = { fg = colors.git_delete }

  hl.LspReferenceText = { bg = colors.bg_visual }
  hl.LspReferenceRead = { bg = colors.bg_visual }
  hl.LspReferenceWrite = { bg = colors.bg_visual }

  hl.LspSignatureActiveParameter = { fg = colors.bright_yellow, bold = true }

  hl.LspInfoBorder = { fg = colors.border, bg = colors.bg_float }

  hl.BlinkCmpMenu = { fg = colors.fg, bg = colors.bg_popup }
  hl.BlinkCmpMenuBorder = { fg = colors.border, bg = colors.bg_popup }
  hl.BlinkCmpMenuSelection = { fg = colors.bg, bg = colors.blue }
  hl.BlinkCmpDoc = { fg = colors.fg, bg = colors.bg_float }
  hl.BlinkCmpDocBorder = { fg = colors.border, bg = colors.bg_float }
  hl.BlinkCmpSignatureHelp = { fg = colors.fg, bg = colors.bg_float }
  hl.BlinkCmpSignatureHelpBorder = { fg = colors.border, bg = colors.bg_float }

  hl.BlinkCmpKindText = { fg = colors.fg }
  hl.BlinkCmpKindMethod = { fg = colors.bright_blue }
  hl.BlinkCmpKindFunction = { fg = colors.bright_blue }
  hl.BlinkCmpKindConstructor = { fg = colors.yellow }
  hl.BlinkCmpKindField = { fg = colors.red }
  hl.BlinkCmpKindVariable = { fg = colors.magenta }
  hl.BlinkCmpKindClass = { fg = colors.yellow }
  hl.BlinkCmpKindInterface = { fg = colors.yellow }
  hl.BlinkCmpKindModule = { fg = colors.cyan }
  hl.BlinkCmpKindProperty = { fg = colors.magenta }
  hl.BlinkCmpKindUnit = { fg = colors.bright_red }
  hl.BlinkCmpKindValue = { fg = colors.bright_red }
  hl.BlinkCmpKindEnum = { fg = colors.yellow }
  hl.BlinkCmpKindKeyword = { fg = colors.magenta }
  hl.BlinkCmpKindSnippet = { fg = colors.green }
  hl.BlinkCmpKindColor = { fg = colors.cyan }
  hl.BlinkCmpKindFile = { fg = colors.blue }
  hl.BlinkCmpKindReference = { fg = colors.cyan }
  hl.BlinkCmpKindFolder = { fg = colors.blue }
  hl.BlinkCmpKindEnumMember = { fg = colors.bright_red }
  hl.BlinkCmpKindConstant = { fg = colors.bright_cyan }
  hl.BlinkCmpKindStruct = { fg = colors.yellow }
  hl.BlinkCmpKindEvent = { fg = colors.magenta }
  hl.BlinkCmpKindOperator = { fg = colors.cyan }
  hl.BlinkCmpKindTypeParameter = { fg = colors.yellow }
  hl.BlinkCmpGhostText = { fg = colors.bright_black }

  hl.MasonHeader = { fg = colors.bg, bg = colors.bright_blue, bold = true }
  hl.MasonHighlight = { fg = colors.bright_blue }
  hl.MasonHighlightBlock = { fg = colors.bg, bg = colors.bright_blue }
  hl.MasonMuted = { fg = colors.comment }
  hl.MasonMutedBlock = { fg = colors.fg, bg = colors.bg_highlight }

  hl.AerialLine = { bg = colors.bg_visual }
  hl.AerialGuide = { fg = colors.fg_gutter }

  hl.WinBar = { fg = colors.bright_white, bg = colors.none }
  hl.WinBarNC = { fg = colors.comment, bg = colors.none }

  hl.DropBarIconUISeparator = { fg = colors.comment }
  hl.DropBarFileName = { fg = colors.bright_white }
  hl.DropBarFileNameModified = { fg = colors.yellow }
  hl.DropBarIconKindFile = { fg = colors.blue }
  hl.DropBarIconKindFolder = { fg = colors.blue }
  hl.DropBarIconKindFunction = { fg = colors.bright_blue }
  hl.DropBarIconKindMethod = { fg = colors.bright_blue }
  hl.DropBarIconKindClass = { fg = colors.yellow }
  hl.DropBarIconKindModule = { fg = colors.cyan }
  hl.DropBarIconKindVariable = { fg = colors.magenta }
  hl.DropBarIconKindConstant = { fg = colors.bright_cyan }
  hl.DropBarIconKindProperty = { fg = colors.magenta }
  hl.DropBarIconKindInterface = { fg = colors.yellow }
  hl.DropBarIconKindStruct = { fg = colors.yellow }

  hl.TroubleNormal = { fg = colors.fg, bg = colors.bg_sidebar }
  hl.TroubleText = { fg = colors.fg_sidebar }
  hl.TroubleCount = { fg = colors.magenta, bg = colors.bg_highlight }
  hl.TroubleCode = { fg = colors.green }

  hl.DiffviewDiffAdd = { link = "DiffAdd" }
  hl.DiffviewDiffDelete = { link = "DiffDelete" }
  hl.DiffviewDiffChange = { link = "DiffChange" }
  hl.DiffviewDiffText = { link = "DiffText" }

  hl.OverseerTask = { fg = colors.fg }
  hl.OverseerTaskBorder = { fg = colors.border }

  hl.SnacksPickerDir = { fg = colors.comment }
  hl.SnacksPickerFile = { fg = colors.fg }
  hl.SnacksPickerBorder = { fg = colors.border, bg = colors.bg_float }
  hl.SnacksPickerTitle = { fg = colors.bright_blue, bold = true }
  hl.SnacksPickerFooter = { fg = colors.comment }
  hl.SnacksPickerPreview = { fg = colors.fg, bg = colors.bg_float }
  hl.SnacksPickerCursor = { fg = colors.bg, bg = colors.blue }
  hl.SnacksPickerMatch = { fg = colors.bright_yellow, bold = true }

  hl.FlashLabel = { fg = colors.bg, bg = colors.bright_magenta, bold = true }
  hl.FlashMatch = { fg = colors.bright_cyan, bg = colors.bg_highlight }
  hl.FlashCurrent = { fg = colors.bright_yellow, bg = colors.bg_highlight, bold = true }
  hl.FlashBackdrop = { fg = colors.comment }
  hl.FlashPrompt = { fg = colors.fg, bg = colors.bg_float }

  hl.TodoBgFIX = { fg = colors.bg, bg = colors.error, bold = true }
  hl.TodoBgHACK = { fg = colors.bg, bg = colors.warning, bold = true }
  hl.TodoBgNOTE = { fg = colors.bg, bg = colors.info, bold = true }
  hl.TodoBgPERF = { fg = colors.bg, bg = colors.magenta, bold = true }
  hl.TodoBgTODO = { fg = colors.bg, bg = colors.hint, bold = true }
  hl.TodoBgWARN = { fg = colors.bg, bg = colors.warning, bold = true }
  hl.TodoFgFIX = { fg = colors.error, bold = true }
  hl.TodoFgHACK = { fg = colors.warning, bold = true }
  hl.TodoFgNOTE = { fg = colors.info, bold = true }
  hl.TodoFgPERF = { fg = colors.magenta, bold = true }
  hl.TodoFgTODO = { fg = colors.hint, bold = true }
  hl.TodoFgWARN = { fg = colors.warning, bold = true }
  hl.TodoSignFIX = { fg = colors.error }
  hl.TodoSignHACK = { fg = colors.warning }
  hl.TodoSignNOTE = { fg = colors.info }
  hl.TodoSignPERF = { fg = colors.magenta }
  hl.TodoSignTODO = { fg = colors.hint }
  hl.TodoSignWARN = { fg = colors.warning }

  hl.BqfPreviewFloat = { fg = colors.fg, bg = colors.bg_float }
  hl.BqfPreviewBorder = { fg = colors.border, bg = colors.bg_float }
  hl.BqfPreviewTitle = { fg = colors.bright_blue, bg = colors.bg_float, bold = true }
  hl.BqfPreviewThumb = { bg = colors.fg_gutter }
  hl.BqfSign = { fg = colors.blue }

  hl.OilDir = { fg = colors.blue }
  hl.OilDirIcon = { fg = colors.blue }
  hl.OilLink = { fg = colors.cyan }
  hl.OilLinkTarget = { fg = colors.cyan }
  hl.OilFile = { fg = colors.fg }
  hl.OilCreate = { fg = colors.green, bold = true }
  hl.OilDelete = { fg = colors.error, bold = true }
  hl.OilMove = { fg = colors.magenta, bold = true }
  hl.OilCopy = { fg = colors.yellow, bold = true }
  hl.OilChange = { fg = colors.warning, bold = true }

  hl.MiniIconsAzure = { fg = colors.blue }
  hl.MiniIconsBlue = { fg = colors.blue }
  hl.MiniIconsCyan = { fg = colors.cyan }
  hl.MiniIconsGreen = { fg = colors.green }
  hl.MiniIconsGrey = { fg = colors.comment }
  hl.MiniIconsOrange = { fg = colors.bright_red }
  hl.MiniIconsPurple = { fg = colors.magenta }
  hl.MiniIconsRed = { fg = colors.red }
  hl.MiniIconsYellow = { fg = colors.yellow }

  hl.MiniPickerBorder = { fg = colors.border, bg = colors.bg_float }
  hl.MiniPickerPrompt = { fg = colors.blue, bg = colors.bg_float }
  hl.MiniPickerMatchCurrent = { bg = colors.bg_visual }
  hl.MiniPickerMatchMarked = { fg = colors.cyan, bold = true }
  hl.MiniPickerMatchRanges = { fg = colors.bright_yellow, bold = true }

  hl.MiniHipatternsFixme = { fg = colors.bg, bg = colors.error, bold = true }
  hl.MiniHipatternsHack = { fg = colors.bg, bg = colors.warning, bold = true }
  hl.MiniHipatternsNote = { fg = colors.bg, bg = colors.info, bold = true }
  hl.MiniHipatternsTodo = { fg = colors.bg, bg = colors.hint, bold = true }

  hl.MiniAlignWindow = { bg = colors.bg_float }

  hl.MatchParen = { fg = colors.bright_yellow, bg = colors.bg_highlight, bold = true }
  hl.MatchWord = { bg = colors.bg_visual }
  hl.MatchParenOffscreen = { fg = colors.comment, bg = colors.bg_highlight }

  hl["@lsp.type.variable"] = { link = "@variable" }
  hl["@lsp.type.parameter"] = { link = "@variable.parameter" }
  hl["@lsp.type.property"] = { link = "@property" }
  hl["@lsp.type.function"] = { link = "@function" }
  hl["@lsp.type.method"] = { link = "@function.method" }
  hl["@lsp.type.class"] = { link = "@type" }
  hl["@lsp.type.struct"] = { link = "@type" }
  hl["@lsp.type.interface"] = { link = "@type" }
  hl["@lsp.type.enum"] = { link = "@type" }
  hl["@lsp.type.type"] = { link = "@type" }
  hl["@lsp.type.typeParameter"] = { link = "@type" }
  hl["@lsp.type.namespace"] = { link = "@module" }
  hl["@lsp.type.keyword"] = { link = "@keyword" }
  hl["@lsp.type.comment"] = { link = "@comment" }
  hl["@lsp.type.operator"] = { link = "@operator" }
  hl["@lsp.type.macro"] = { link = "@constant.macro" }
  hl["@lsp.type.string"] = { link = "@string" }
  hl["@lsp.type.number"] = { link = "@number" }
  hl["@lsp.type.boolean"] = { link = "@boolean" }
  hl["@lsp.type.decorator"] = { link = "@attribute" }
  hl["@lsp.mod.readonly"] = { link = "@constant" }
  hl["@lsp.mod.deprecated"] = { fg = colors.comment, strikethrough = true }

  return hl
end

return M

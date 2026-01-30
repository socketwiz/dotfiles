-- midnight.lua — cold blue-only colorscheme (tokyonight structure, no warm colors)
vim.cmd("hi clear")
vim.g.colors_name = "midnight"
vim.o.termguicolors = true
vim.o.background = "dark"

-- helpers ----------------------------------------------------------------

---Blend a hex color toward a base by a 0-1 ratio
---@param fg string hex "#rrggbb"
---@param bg string hex "#rrggbb"
---@param alpha number 0-1 (0 = bg, 1 = fg)
---@return string
local function blend(fg, bg, alpha)
  local function hex(c) return tonumber(c, 16) end
  local r = math.floor(hex(fg:sub(2, 3)) * alpha + hex(bg:sub(2, 3)) * (1 - alpha))
  local g = math.floor(hex(fg:sub(4, 5)) * alpha + hex(bg:sub(4, 5)) * (1 - alpha))
  local b = math.floor(hex(fg:sub(6, 7)) * alpha + hex(bg:sub(6, 7)) * (1 - alpha))
  return string.format("#%02x%02x%02x", r, g, b)
end

local function hi(name, opts)
  vim.api.nvim_set_hl(0, name, opts)
end

-- palette ----------------------------------------------------------------

local c = {
  none    = "NONE",
  bg      = "#000000",
  bg_dark = "#000000",
  bg_float = "#000000",
  bg_popup = "#0d0d1a",
  bg_sidebar = "#000000",
  bg_highlight = "#0a0a2a",
  bg_visual = "#283457",
  bg_search = "#3d59a1",

  fg       = "#becdff",
  fg_dark  = "#a2b1ea",
  fg_gutter = "#3b4261",

  -- blues / purples / cyans (max saturation)
  blue     = "#90b8ff",
  blue0    = "#0b42d2",
  blue1    = "#46ffdb",
  blue2    = "#0db9d7",
  blue5    = "#a0e8ff",
  blue6    = "#aefffd",
  blue7    = "#394b70",
  cyan     = "#83e2ff",
  purple   = "#d2a8ff",
  magenta  = "#cdb0ff",
  magenta2 = "#ff007c",
  pink     = "#ff92d0",

  -- warm colors remapped to blue family
  orange   = "#90b8ff",
  yellow   = "#a0e8ff",
  green    = "#83e2ff",
  green1   = "#83e2ff",
  green2   = "#46ffdb",
  teal     = "#46ffdb",

  -- reds kept for errors/danger
  red      = "#ff6e88",
  red1     = "#ff2626",

  -- ui
  border   = "#3b4261",
  comment  = "#4255c5",
  dark3    = "#545c7e",
  dark5    = "#5b69b9",
  terminal_black = "#414868",
  git_add    = "#83e2ff",
  git_change = "#90b8ff",
  git_delete = "#ff6e88",
  diff_add   = "#83e2ff",
  diff_change = "#90b8ff",
  diff_delete = "#ff6e88",
  diff_text   = "#a0e8ff",

  -- rainbow (markdown headings, etc.)
  rainbow = { "#90b8ff", "#a0e8ff", "#83e2ff", "#46ffdb", "#cdb0ff", "#ff92d0" },
}

-- derived colors
local c_diag_error_bg = blend(c.red, c.bg, 0.10)
local c_diag_warn_bg  = blend(c.blue, c.bg, 0.10)
local c_diag_info_bg  = blend(c.cyan, c.bg, 0.10)
local c_diag_hint_bg  = blend(c.blue, c.bg, 0.10)
local c_diff_add_bg   = blend(c.green, c.bg, 0.15)
local c_diff_change_bg = blend(c.blue, c.bg, 0.15)
local c_diff_delete_bg = blend(c.red, c.bg, 0.15)
local c_diff_text_bg  = blend(c.blue, c.bg, 0.30)
local c_inline_code_bg = "#1a1a3a"

-- editor UI --------------------------------------------------------------

hi("Normal",           { fg = c.fg, bg = c.bg })
hi("NormalNC",         { fg = c.fg, bg = c.bg })
hi("NormalFloat",      { fg = c.fg, bg = c.bg_float })
hi("NormalSB",         { fg = c.fg_dark, bg = c.bg_sidebar })
hi("ColorColumn",      { bg = c.bg_highlight })
hi("Conceal",          { fg = c.dark5 })
hi("Cursor",           { fg = c.bg, bg = c.fg })
hi("lCursor",          { fg = c.bg, bg = c.fg })
hi("CursorIM",         { fg = c.bg, bg = c.fg })
hi("CursorColumn",     { bg = c.bg_highlight })
hi("CursorLine",       { bg = c.bg_highlight })
hi("CursorLineNr",     { fg = c.blue, bold = true })
hi("Directory",        { fg = c.blue })
hi("DiffAdd",          { bg = c_diff_add_bg })
hi("DiffChange",       { bg = c_diff_change_bg })
hi("DiffDelete",       { bg = c_diff_delete_bg })
hi("DiffText",         { bg = c_diff_text_bg })
hi("EndOfBuffer",      { fg = c.bg })
hi("ErrorMsg",         { fg = c.red })
hi("VertSplit",        { fg = c.border })
hi("WinSeparator",     { fg = c.border, bold = true })
hi("Folded",           { fg = c.blue, bg = c.fg_gutter })
hi("FoldColumn",       { fg = c.comment, bg = c.bg })
hi("SignColumn",       { fg = c.fg_gutter, bg = c.bg })
hi("IncSearch",        { fg = c.bg, bg = c.blue })
hi("Substitute",       { fg = c.bg, bg = c.red })
hi("LineNr",           { fg = c.fg_gutter })
hi("LineNrAbove",      { fg = c.fg_gutter })
hi("LineNrBelow",      { fg = c.fg_gutter })
hi("MatchParen",       { fg = c.blue, bold = true })
hi("ModeMsg",          { fg = c.fg_dark, bold = true })
hi("MsgArea",          { fg = c.fg_dark })
hi("MoreMsg",          { fg = c.blue })
hi("NonText",          { fg = c.dark3 })
hi("Pmenu",            { fg = c.fg, bg = c.bg_popup })
hi("PmenuSel",         { bg = c.bg_visual })
hi("PmenuSbar",        { bg = blend(c.bg_popup, c.fg, 0.05) })
hi("PmenuThumb",       { bg = c.fg_gutter })
hi("Question",         { fg = c.blue })
hi("QuickFixLine",     { bg = c.bg_visual, bold = true })
hi("Search",           { fg = c.fg, bg = c.bg_search })
hi("CurSearch",        { link = "IncSearch" })
hi("SpecialKey",       { fg = c.dark3 })
hi("SpellBad",         { sp = c.red, undercurl = true })
hi("SpellCap",         { sp = c.blue, undercurl = true })
hi("SpellLocal",       { sp = c.cyan, undercurl = true })
hi("SpellRare",        { sp = c.purple, undercurl = true })
hi("StatusLine",       { fg = c.fg_dark, bg = c.bg })
hi("StatusLineNC",     { fg = c.fg_gutter, bg = c.bg })
hi("TabLine",          { fg = c.fg_gutter, bg = c.bg })
hi("TabLineFill",      { bg = c.bg })
hi("TabLineSel",       { fg = c.fg, bg = c.blue7 })
hi("Title",            { fg = c.blue, bold = true })
hi("Visual",           { bg = c.bg_visual })
hi("VisualNOS",        { bg = c.bg_visual })
hi("WarningMsg",       { fg = c.blue })
hi("Whitespace",       { fg = c.fg_gutter })
hi("WildMenu",        { bg = c.bg_visual })
hi("WinBar",           { link = "StatusLine" })
hi("WinBarNC",         { link = "StatusLineNC" })
hi("FloatBorder",      { fg = c.border, bg = c.bg_float })
hi("FloatTitle",       { fg = c.blue, bg = c.bg_float })

-- syntax -----------------------------------------------------------------

hi("Comment",          { fg = c.comment, italic = true })
hi("Constant",         { fg = c.blue, bold = true })
hi("String",           { fg = c.cyan, italic = true })
hi("Character",        { fg = c.cyan })
hi("Number",           { fg = c.blue })
hi("Boolean",          { fg = c.blue })
hi("Float",            { fg = c.blue })
hi("Identifier",       { fg = c.magenta })
hi("Function",         { fg = c.pink, bold = true })
hi("Statement",        { fg = c.magenta, bold = true })
hi("Conditional",      { fg = c.magenta })
hi("Repeat",           { fg = c.magenta })
hi("Label",            { fg = c.blue })
hi("Operator",         { fg = c.blue5 })
hi("Keyword",          { fg = c.purple, bold = true, italic = true })
hi("Exception",        { fg = c.magenta })
hi("PreProc",          { fg = c.cyan })
hi("Include",          { fg = c.blue })
hi("Define",           { fg = c.magenta })
hi("Macro",            { fg = c.magenta })
hi("PreCondit",        { fg = c.cyan })
hi("Type",             { fg = c.blue1, bold = true })
hi("StorageClass",     { fg = c.blue })
hi("Structure",        { fg = c.blue })
hi("Typedef",          { fg = c.blue })
hi("Special",          { fg = c.blue5 })
hi("SpecialChar",      { fg = c.blue5 })
hi("Tag",              { fg = c.blue })
hi("Delimiter",        { fg = c.blue5 })
hi("SpecialComment",   { fg = c.blue })
hi("Debug",            { fg = c.blue })
hi("Underlined",       { underline = true })
hi("Bold",             { bold = true })
hi("Italic",           { italic = true })
hi("Ignore",           { })
hi("Error",            { fg = c.red })
hi("Todo",             { fg = c.bg, bg = c.blue })

-- treesitter -------------------------------------------------------------

hi("@variable",                   { fg = c.fg })
hi("@variable.builtin",           { fg = c.red })
hi("@variable.parameter",         { fg = c.blue5 })
hi("@variable.parameter.builtin", { fg = c.blue5 })
hi("@variable.member",            { fg = c.cyan })

hi("@constant",          { fg = c.blue, bold = true })
hi("@constant.builtin",  { fg = c.blue, bold = true })
hi("@constant.macro",    { fg = c.magenta })

hi("@module",             { fg = c.blue5 })
hi("@module.builtin",     { fg = c.blue5 })
hi("@label",              { fg = c.blue })

hi("@string",             { fg = c.cyan, italic = true })
hi("@string.documentation", { fg = c.blue5 })
hi("@string.regexp",      { fg = c.blue6 })
hi("@string.escape",      { fg = c.pink })
hi("@string.special",     { fg = c.blue5 })
hi("@string.special.symbol", { fg = c.blue })
hi("@string.special.url", { fg = c.blue, underline = true })

hi("@character",          { fg = c.cyan })
hi("@character.special",  { fg = c.blue5 })

hi("@boolean",            { fg = c.blue })
hi("@number",             { fg = c.blue })
hi("@number.float",       { fg = c.blue })

hi("@type",               { fg = c.blue1, bold = true })
hi("@type.builtin",       { fg = blend(c.blue1, c.fg, 0.8), bold = true })
hi("@type.definition",    { fg = c.blue1, bold = true })

hi("@attribute",           { fg = c.blue })
hi("@attribute.builtin",   { fg = c.blue })
hi("@property",            { fg = c.cyan })

hi("@function",            { fg = c.pink, bold = true })
hi("@function.builtin",    { fg = c.pink })
hi("@function.call",       { fg = c.pink })
hi("@function.macro",      { fg = c.magenta })
hi("@function.method",     { fg = c.pink })
hi("@function.method.call", { fg = c.pink })

hi("@constructor",         { fg = c.pink })

hi("@operator",            { fg = c.blue5 })

hi("@keyword",             { fg = c.purple, bold = true, italic = true })
hi("@keyword.coroutine",   { fg = c.purple, bold = true, italic = true })
hi("@keyword.function",    { fg = c.pink })
hi("@keyword.operator",    { fg = c.blue5 })
hi("@keyword.import",      { fg = c.blue })
hi("@keyword.type",        { fg = c.blue1 })
hi("@keyword.modifier",    { fg = c.purple, bold = true, italic = true })
hi("@keyword.repeat",      { fg = c.magenta })
hi("@keyword.return",      { fg = c.magenta })
hi("@keyword.debug",       { fg = c.blue })
hi("@keyword.exception",   { fg = c.magenta })
hi("@keyword.conditional",         { fg = c.magenta })
hi("@keyword.conditional.ternary", { fg = c.blue5 })
hi("@keyword.directive",          { fg = c.cyan })
hi("@keyword.directive.define",   { fg = c.magenta })

hi("@punctuation.delimiter",  { fg = c.blue5 })
hi("@punctuation.bracket",    { fg = c.fg_dark })
hi("@punctuation.special",    { fg = c.blue5 })

hi("@comment",              { fg = c.comment, italic = true })
hi("@comment.documentation", { fg = c.blue5 })
hi("@comment.error",        { fg = c.red })
hi("@comment.warning",      { fg = c.blue })
hi("@comment.todo",         { fg = c.bg, bg = c.blue })
hi("@comment.note",         { fg = c.cyan })

-- markup
hi("@markup.strong",         { bold = true })
hi("@markup.italic",         { italic = true })
hi("@markup.strikethrough",  { strikethrough = true })
hi("@markup.underline",      { underline = true })
hi("@markup.link",           { fg = c.blue1 })
hi("@markup.link.label",     { fg = c.blue, underline = true })
hi("@markup.link.label.symbol", { fg = c.blue })
hi("@markup.link.url",       { fg = c.blue, underline = true })
hi("@markup.raw",            { fg = c.cyan })
hi("@markup.raw.markdown_inline", { bg = c_inline_code_bg })
hi("@markup.math",           { fg = c.blue })
hi("@markup.list",           { fg = c.blue5 })
hi("@markup.list.checked",   { fg = c.blue })
hi("@markup.list.unchecked", { fg = c.dark5 })
hi("@markup.environment",    { fg = c.magenta })
hi("@markup.environment.name", { fg = c.blue1 })

-- markdown heading rainbow
for i, color in ipairs(c.rainbow) do
  hi("@markup.heading." .. i, { fg = color, bold = true })
  hi("@markup.heading." .. i .. ".markdown", { fg = color, bold = true })
end
hi("@markup.heading",        { fg = c.blue, bold = true })

hi("@diff.plus",             { fg = c.git_add })
hi("@diff.minus",            { fg = c.git_delete })
hi("@diff.delta",            { fg = c.git_change })

hi("@tag",                   { fg = c.pink })
hi("@tag.attribute",         { fg = c.cyan })
hi("@tag.delimiter",         { fg = c.blue5 })

-- LSP highlights ---------------------------------------------------------

hi("LspReferenceText",       { bg = c.fg_gutter })
hi("LspReferenceRead",       { bg = c.fg_gutter })
hi("LspReferenceWrite",      { bg = c.fg_gutter })
hi("LspSignatureActiveParameter", { bg = c.bg_visual, bold = true })
hi("LspCodeLens",            { fg = c.comment })
hi("LspInlayHint",           { fg = c.dark3, bg = blend(c.dark3, c.bg, 0.10) })
hi("LspInfoBorder",          { fg = c.border })

-- diagnostics
hi("DiagnosticError",           { fg = "#ff0000" })
hi("DiagnosticWarn",            { fg = c.blue })
hi("DiagnosticInfo",            { fg = c.cyan })
hi("DiagnosticHint",            { fg = c.blue })
hi("DiagnosticUnnecessary",     { fg = c.dark3 })
hi("DiagnosticVirtualTextError", { fg = c.red, bg = c_diag_error_bg })
hi("DiagnosticVirtualTextWarn",  { fg = c.blue, bg = c_diag_warn_bg })
hi("DiagnosticVirtualTextInfo",  { fg = c.cyan, bg = c_diag_info_bg })
hi("DiagnosticVirtualTextHint",  { fg = c.blue, bg = c_diag_hint_bg })
hi("DiagnosticUnderlineError",  { sp = c.red, undercurl = true })
hi("DiagnosticUnderlineWarn",   { sp = c.blue, undercurl = true })
hi("DiagnosticUnderlineInfo",   { sp = c.cyan, undercurl = true })
hi("DiagnosticUnderlineHint",   { sp = c.blue, undercurl = true })

-- semantic tokens (@lsp.type.* and @lsp.typemod.*)
hi("@lsp.type.boolean",         { link = "@boolean" })
hi("@lsp.type.builtinType",     { link = "@type.builtin" })
hi("@lsp.type.comment",         { link = "@comment" })
hi("@lsp.type.decorator",       { link = "@attribute" })
hi("@lsp.type.deriveHelper",    { link = "@attribute" })
hi("@lsp.type.enum",            { link = "@type" })
hi("@lsp.type.enumMember",      { link = "@constant" })
hi("@lsp.type.escapeSequence",  { link = "@string.escape" })
hi("@lsp.type.formatSpecifier", { link = "@punctuation.special" })
hi("@lsp.type.generic",         { link = "@variable" })
hi("@lsp.type.interface",       { fg = blend(c.blue1, c.fg, 0.7) })
hi("@lsp.type.keyword",         { link = "@keyword" })
hi("@lsp.type.lifetime",        { link = "@keyword.modifier" })
hi("@lsp.type.namespace",       { link = "@module" })
hi("@lsp.type.number",          { link = "@number" })
hi("@lsp.type.operator",        { link = "@operator" })
hi("@lsp.type.parameter",       { link = "@variable.parameter" })
hi("@lsp.type.property",        { link = "@property" })
hi("@lsp.type.selfKeyword",     { link = "@variable.builtin" })
hi("@lsp.type.selfTypeKeyword", { link = "@variable.builtin" })
hi("@lsp.type.string",          { link = "@string" })
hi("@lsp.type.typeAlias",       { link = "@type.definition" })
hi("@lsp.type.unresolvedReference", { sp = c.red, undercurl = true })
hi("@lsp.type.variable",        { })  -- defer to treesitter
hi("@lsp.typemod.class.defaultLibrary",    { link = "@type.builtin" })
hi("@lsp.typemod.enum.defaultLibrary",     { link = "@type.builtin" })
hi("@lsp.typemod.enumMember.defaultLibrary", { link = "@constant.builtin" })
hi("@lsp.typemod.function.defaultLibrary", { link = "@function.builtin" })
hi("@lsp.typemod.keyword.async",           { link = "@keyword.coroutine" })
hi("@lsp.typemod.keyword.injected",        { link = "@keyword" })
hi("@lsp.typemod.macro.defaultLibrary",    { link = "@function.builtin" })
hi("@lsp.typemod.method.defaultLibrary",   { link = "@function.builtin" })
hi("@lsp.typemod.operator.injected",       { link = "@operator" })
hi("@lsp.typemod.string.injected",         { link = "@string" })
hi("@lsp.typemod.struct.defaultLibrary",   { link = "@type.builtin" })
hi("@lsp.typemod.type.defaultLibrary",     { link = "@type.builtin" })
hi("@lsp.typemod.typeAlias.defaultLibrary", { link = "@type.builtin" })
hi("@lsp.typemod.variable.callable",       { link = "@function" })
hi("@lsp.typemod.variable.defaultLibrary", { link = "@variable.builtin" })
hi("@lsp.typemod.variable.injected",       { link = "@variable" })
hi("@lsp.typemod.variable.static",         { link = "@constant" })

-- LSP Kind / CmpItemKind ------------------------------------------------

local kind_colors = {
  Array         = c.blue,
  Boolean       = c.blue,
  Class         = c.blue1,
  Color         = c.cyan,
  Constant      = c.blue,
  Constructor   = c.pink,
  Copilot       = c.blue1,
  Default       = c.blue,
  Enum          = c.blue1,
  EnumMember    = c.cyan,
  Event         = c.blue,
  Field         = c.cyan,
  File          = c.fg,
  Folder        = c.blue,
  Function      = c.pink,
  Interface     = c.blue1,
  Key           = c.purple,
  Keyword       = c.purple,
  Method        = c.pink,
  Module        = c.blue5,
  Namespace     = c.blue5,
  Null          = c.blue,
  Number        = c.blue,
  Object        = c.blue,
  Operator      = c.blue5,
  Package       = c.blue5,
  Property      = c.cyan,
  Reference     = c.blue,
  Snippet       = c.blue1,
  String        = c.cyan,
  Struct        = c.blue,
  Text          = c.fg,
  TypeParameter = c.blue1,
  Unit          = c.blue1,
  Value         = c.blue,
  Variable      = c.magenta,
}

for kind, color in pairs(kind_colors) do
  hi("CmpItemKind" .. kind, { fg = color })
  hi("LspKind" .. kind, { fg = color })
end

hi("CmpItemAbbrDeprecated",  { fg = c.fg_gutter, strikethrough = true })
hi("CmpItemAbbrMatch",       { fg = c.blue, bold = true })
hi("CmpItemAbbrMatchFuzzy",  { fg = c.blue, bold = true })
hi("CmpItemMenu",            { fg = c.comment })

-- plugins ----------------------------------------------------------------

-- gitsigns
hi("GitSignsAdd",          { fg = c.git_add })
hi("GitSignsChange",       { fg = c.git_change })
hi("GitSignsDelete",       { fg = c.git_delete })
hi("GitSignsCurrentLineBlame", { fg = c.dark3 })

-- telescope
hi("TelescopeNormal",       { fg = c.fg, bg = c.bg })
hi("TelescopeBorder",       { fg = c.border, bg = c.bg })
hi("TelescopeTitle",        { fg = c.blue, bold = true })
hi("TelescopePromptNormal", { fg = c.fg, bg = c.bg })
hi("TelescopePromptBorder", { fg = c.border, bg = c.bg })
hi("TelescopePromptTitle",  { fg = c.blue, bold = true })
hi("TelescopePreviewTitle", { fg = c.blue, bold = true })
hi("TelescopeResultsTitle", { fg = c.blue, bold = true })
hi("TelescopeMatching",     { fg = c.blue, bold = true })
hi("TelescopeSelection",    { bg = c.bg_visual })
hi("TelescopeSelectionCaret", { fg = c.blue })

-- nvim-cmp
hi("CmpDocumentation",       { fg = c.fg, bg = c.bg_float })
hi("CmpDocumentationBorder", { fg = c.border, bg = c.bg_float })
hi("CmpGhostText",           { fg = c.dark3 })

-- lazy.nvim
hi("LazyH1",              { fg = c.bg, bg = c.blue, bold = true })
hi("LazyButton",          { fg = c.fg, bg = c.fg_gutter })
hi("LazyButtonActive",    { fg = c.bg, bg = c.blue })
hi("LazySpecial",         { fg = c.blue })
hi("LazyProgressDone",    { fg = c.blue })
hi("LazyProgressTodo",    { fg = c.fg_gutter })
hi("LazyReasonPlugin",    { fg = c.blue })
hi("LazyReasonEvent",     { fg = c.blue5 })
hi("LazyReasonCmd",       { fg = c.magenta })
hi("LazyReasonFt",        { fg = c.cyan })
hi("LazyReasonKeys",      { fg = c.purple })
hi("LazyReasonStart",     { fg = c.blue })
hi("LazyReasonRuntime",   { fg = c.blue5 })

-- indent-blankline / mini.indentscope
hi("IblIndent",            { fg = c.fg_gutter })
hi("IblScope",             { fg = c.blue })
hi("MiniIndentscopeSymbol", { fg = c.blue })

-- which-key
hi("WhichKey",             { fg = c.cyan })
hi("WhichKeyGroup",        { fg = c.blue })
hi("WhichKeyDesc",         { fg = c.magenta })
hi("WhichKeySeparator",    { fg = c.comment })
hi("WhichKeyFloat",        { bg = c.bg_sidebar })
hi("WhichKeyValue",        { fg = c.dark5 })

-- notify
hi("NotifyERRORBorder",    { fg = c.red })
hi("NotifyWARNBorder",     { fg = c.blue })
hi("NotifyINFOBorder",     { fg = c.cyan })
hi("NotifyDEBUGBorder",    { fg = c.comment })
hi("NotifyTRACEBorder",    { fg = c.purple })
hi("NotifyERRORIcon",      { fg = c.red })
hi("NotifyWARNIcon",       { fg = c.blue })
hi("NotifyINFOIcon",       { fg = c.cyan })
hi("NotifyDEBUGIcon",      { fg = c.comment })
hi("NotifyTRACEIcon",      { fg = c.purple })
hi("NotifyERRORTitle",     { fg = c.red })
hi("NotifyWARNTitle",      { fg = c.blue })
hi("NotifyINFOTitle",      { fg = c.cyan })
hi("NotifyDEBUGTitle",     { fg = c.comment })
hi("NotifyTRACETitle",     { fg = c.purple })

-- terminal colors --------------------------------------------------------

vim.g.terminal_color_0  = "#414868"
vim.g.terminal_color_1  = "#ff6e88"
vim.g.terminal_color_2  = "#83e2ff"
vim.g.terminal_color_3  = "#a0e8ff"
vim.g.terminal_color_4  = "#90b8ff"
vim.g.terminal_color_5  = "#cdb0ff"
vim.g.terminal_color_6  = "#46ffdb"
vim.g.terminal_color_7  = "#becdff"
vim.g.terminal_color_8  = "#545c7e"
vim.g.terminal_color_9  = "#ff6e88"
vim.g.terminal_color_10 = "#83e2ff"
vim.g.terminal_color_11 = "#a0e8ff"
vim.g.terminal_color_12 = "#90b8ff"
vim.g.terminal_color_13 = "#cdb0ff"
vim.g.terminal_color_14 = "#46ffdb"
vim.g.terminal_color_15 = "#becdff"

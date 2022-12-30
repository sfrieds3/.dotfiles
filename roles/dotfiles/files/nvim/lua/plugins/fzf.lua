local M = {
  "ibhagwan/fzf-lua",
}

function M.config()
  vim.keymap.set("n", "<Leader>f;", require("fzf-lua").resume, { desc = "FzfLua: [f]zf resume" })
  vim.keymap.set("n", "<Leader>f/", require("fzf-lua").blines, { desc = "FzfLua: [f]zf buffer lines" })
  vim.keymap.set("n", "<Leader>ff", require("fzf-lua").files, { desc = "FzfLua: [f]zf find [f]iles" })
  vim.keymap.set("n", "<Leader>fb", require("fzf-lua").buffers, { desc = "FzfLua: [f]zf [b]uffers" })
  vim.keymap.set("n", "<Leader>fl", require("fzf-lua").lines, { desc = "FzfLua: [f]zf [l]ines" })
  vim.keymap.set("n", "<Leader>fo", require("fzf-lua").oldfiles, { desc = "FzfLua: [f]zf [o]ldfiles" })
  vim.keymap.set("n", "<Leader>fQ", require("fzf-lua").quickfix, { desc = "FzfLua: [f]zf [Q]uickfix" })
  vim.keymap.set("n", "<Leader>fl", require("fzf-lua").loclist, { desc = "FzfLua: [f]zf [L]oclist" })
  vim.keymap.set("n", "<Leader>fgg", require("fzf-lua").grep, { desc = "FzfLua: [f]zf [g]rep" })
  vim.keymap.set("n", "<Leader>fgw", require("fzf-lua").grep_cword, { desc = "FzfLua: [f]zf [g]rep [w]ord" })
  vim.keymap.set("n", "<Leader>fgW", require("fzf-lua").grep_cWORD, { desc = "FzfLua: [f]zf [g]rep [W]ORD" })
  vim.keymap.set(
    "n",
    "<Leader>fgv",
    require("fzf-lua").grep_visual,
    { desc = "FzfLua: [f]zf [g]rep [v]isual selection" }
  )
  vim.keymap.set("n", "<Leader>flb", require("fzf-lua").lgrep_curbuf, { desc = "FzfLua: [f]zf [l]ive grep buffer" })
  vim.keymap.set("n", "<Leader>fgb", require("fzf-lua").grep_curbuf, { desc = "FzfLua: [f]zf [g]rep [b]uffer" })
  vim.keymap.set("n", "<Leader>fG", require("fzf-lua").live_grep_native, { desc = "FzfLua: [f]zf live [G]rep native" })
  vim.keymap.set("n", "<Leader>fgb", require("fzf-lua").lgrep_curbuf, { desc = "FzfLua: [f]zf [g]rep [b]uffer" })
  vim.keymap.set(
    "n",
    "<Leader>fwD",
    require("fzf-lua").lsp_workspace_diagnostics,
    { desc = "FzfLua: [l]sp view [w]orkspace [D]iagnostics" }
  )
  vim.keymap.set(
    "n",
    "<Leader>fwd",
    require("fzf-lua").lsp_document_diagnostics,
    { desc = "FzfLua: [f]zf lsp vie[w] [d]ocument diagnostics" }
  )
  vim.keymap.set(
    "n",
    "<Leader>fvr",
    require("fzf-lua").lsp_references,
    { desc = "FzfLua: [f]zf lsp [v]iew [r]eferences" }
  )
  vim.keymap.set(
    "n",
    "<Leader>fvd",
    require("fzf-lua").lsp_definitions,
    { desc = "FzfLua: [f]zf lsp [v]iew [d]efinitions" }
  )
  vim.keymap.set(
    "n",
    "<Leader>fvD",
    require("fzf-lua").lsp_declarations,
    { desc = "FzfLua: [f]zf lsp [v]iew [D]eclarations" }
  )
  vim.keymap.set(
    "n",
    "<Leader>fvi",
    require("fzf-lua").lsp_implementations,
    { desc = "FzfLua: [f]zf lsp [v]iew [i]mplementations" }
  )
  vim.keymap.set(
    "n",
    "<Leader>fvs",
    require("fzf-lua").lsp_document_symbols,
    { desc = "FzfLua: [f]zf lsp [v]iew document [s]ymbols" }
  )
  vim.keymap.set(
    "n",
    "<Leader>fvS",
    require("fzf-lua").lsp_live_workspace_symbols,
    { desc = "FzfLua: [f]zf lsp [v]iew workspace [S]ymbols" }
  )
  vim.keymap.set(
    "n",
    "<Leader>fvA",
    require("fzf-lua").lsp_workspace_symbols,
    { desc = "FzfLua: [f]zf lsp [v]iew [A]ll workspace symbols" }
  )
  vim.keymap.set(
    "n",
    "<Leader>fva",
    require("fzf-lua").lsp_code_actions,
    { desc = "FzfLua: [f]zf lsp [v]iew code [a]ctions" }
  )
  vim.keymap.set(
    "n",
    "<Leader>fvc",
    require("fzf-lua").lsp_incoming_calls,
    { desc = "FzfLua: [f]zf lsp [v]iew incoming [c]alls" }
  )
  vim.keymap.set(
    "n",
    "<Leader>fvC",
    require("fzf-lua").lsp_outgoing_calls,
    { desc = "FzfLua: [f]zf lsp [v]iew outgoing [C]alls" }
  )
  vim.keymap.set("n", "<Leader>fm", require("fzf-lua").marks, { desc = "FzfLua: [f]zf [m]arks" })
  vim.keymap.set("n", "<Leader>fr", require("fzf-lua").registers, { desc = "FzfLua: [f]zf [r]egistersj" })
  vim.keymap.set("n", "<Leader>fc", require("fzf-lua").changes, { desc = "FzfLua: [f]zf [c]hanges" })
end

return M

local M = {
  "andymass/vim-matchup",
}

function M.config()
  vim.g.matchup_matchparen_offscreen = { method = "popup" }
  vim.g.matchup_matchparen_deferred_show_delay = 500
  vim.g.matchup_matchparen_deferred_hide_delay = 500
  vim.g.matchup_matchparen_timeout = 100
  vim.g.matchup_matchparen_deferred = 1
end

return M

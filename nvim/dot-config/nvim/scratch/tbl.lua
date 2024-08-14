local nav_hunk_config = {
  wrap = true,
  navigation_message = true,
  preview = true,
}

local nav_hunk_all_config = { target = "all" }
vim.tbl_deep_extend("keep", nav_hunk_all_config, nav_hunk_config)

P(nav_hunk_all_config)

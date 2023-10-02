local M = {
  "nvim-lualine/lualine.nvim",
  dependencies = "nvim-tree/nvim-web-devicons",
}

function M.config()
  --- @param trunc_width number trunctates component when screen width is less then trunc_width
  --- @param trunc_len number truncates component to trunc_len number of chars
  --- @param hide_width number hides component when window width is smaller then hide_width
  --- @param no_ellipsis boolean whether to disable adding '...' at end after truncation
  --- return function that can format the component accordingly
  local function trunc(trunc_width, trunc_len, hide_width, no_ellipsis)
    return function(str)
      local win_width = vim.fn.winwidth(0)
      if hide_width and win_width < hide_width then
        return ""
      elseif trunc_width and trunc_len and win_width < trunc_width and #str > trunc_len then
        return str:sub(1, trunc_len) .. (no_ellipsis and "" or "...")
      end
      return str
    end
  end

  local function diff_source()
    local gitsigns = vim.b.gitsigns_status_dict
    if gitsigns then
      return {
        added = gitsigns.added,
        modified = gitsigns.changed,
        removed = gitsigns.removed,
      }
    end
  end

  local function session_name()
    return require("possession.session").session_name or ""
  end

  require("lualine").setup({
    options = {
      icons_enabled = true,
      theme = "auto",
      section_separators = { left = "", right = "" }, -- left = "", right = ""
      component_separators = { left = "", right = "" }, -- left = "", right = ""
      disabled_filetypes = {},
      always_divide_middle = true,
      globalstatus = true,
    },
    sections = {
      lualine_a = { "mode" },
      lualine_b = {
        { "b:gitsigns_head", icon = "λ", fmt = trunc(50, 15, 50, false) },
        { "diff", source = diff_source, fmt = trunc(80, 10, 80, true) },
        { "diagnostics", fmt = trunc(80, 15, 80, false) },
      },
      lualine_c = {
        {
          "filename",
          path = 3,
          symbols = {
            modified = " ●",
            readonly = " ✘",
          },
        },
        { session_name, icon = " 📌" },
      },
      lualine_x = {
        { "encoding", fmt = trunc(80, 10, 80, true) },
        { "fileformat", fmt = trunc(80, 10, 80, true) },
        { "filetype", fmt = trunc(40, 10, 40, true) },
      },
      lualine_y = { "progress" },
      lualine_z = {
        function()
          return "ℓ:%l 𝚌:%c"
        end,
      },
    },
    inactive_sections = {
      lualine_a = {},
      lualine_b = {},
      lualine_c = {
        { "filename", path = 2 },
      },
      lualine_x = { "location" },
      lualine_y = {},
      lualine_z = {},
    },
    extensions = {
      "aerial",
      "fugitive",
      "fzf",
      "lazy",
      "neo-tree",
      "nvim-dap-ui",
      "overseer",
      "quickfix",
      "symbols-outline",
      "toggleterm",
      "trouble",
    },
  })
end

return M

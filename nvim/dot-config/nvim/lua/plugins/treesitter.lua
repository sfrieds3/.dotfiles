local select = function(query)
  return function()
    require("nvim-treesitter-textobjects.select").select_textobject(query, "textobjects")
  end
end

local move = function(fn, query)
  return function()
    require("nvim-treesitter-textobjects.move")[fn](query, "textobjects")
  end
end

local swap = function(fn, query)
  return function()
    require("nvim-treesitter-textobjects.swap")[fn](query)
  end
end

local current_node = nil

local function incremental_select()
  local node
  if current_node then
    node = current_node:parent()
  else
    node = vim.treesitter.get_node()
  end
  if not node then
    return
  end
  current_node = node
  local sr, sc, er, ec = node:range()
  vim.fn.setpos("'<", { 0, sr + 1, sc + 1, 0 })
  vim.fn.setpos("'>", { 0, er + 1, ec, 0 })
  vim.cmd("normal! gv")
end

local function decremental_select()
  if not current_node then
    return
  end
  local child = current_node:child(0)
  if child then
    current_node = child
  else
    return
  end
  local sr, sc, er, ec = current_node:range()
  vim.fn.setpos("'<", { 0, sr + 1, sc + 1, 0 })
  vim.fn.setpos("'>", { 0, er + 1, ec, 0 })
  vim.cmd("normal! gv")
end

vim.api.nvim_create_autocmd("ModeChanged", {
  pattern = "*:n",
  callback = function()
    current_node = nil
  end,
})

return {
  {
    "nvim-treesitter/nvim-treesitter",
    branch = "main",
    lazy = false,
    build = ":TSUpdate",
    config = function()
      require("nvim-treesitter").setup({
        install_dir = vim.fn.stdpath("data") .. "/site",
      })
      require("nvim-treesitter").install("all")
    end,
    keys = {
      { "<CR>", incremental_select, mode = { "n", "x" }, desc = "Incremental selection" },
      { "<BS>", decremental_select, mode = "x", desc = "Decremental selection" },
    },
  },
  {
    "nvim-treesitter/nvim-treesitter-textobjects",
    branch = "main",
    lazy = false,
    config = function()
      require("nvim-treesitter-textobjects").setup({
        select = { lookahead = true },
        move = { set_jumps = true },
      })
    end,
    keys = {
      { "aa", select("@parameter.outer"), mode = { "x", "o" } },
      { "ia", select("@parameter.inner"), mode = { "x", "o" } },
      { "am", select("@function.outer"), mode = { "x", "o" } },
      { "im", select("@function.inner"), mode = { "x", "o" } },
      { "ac", select("@class.outer"), mode = { "x", "o" } },
      { "ic", select("@class.inner"), mode = { "x", "o" } },
      { "al", select("@loop.outer"), mode = { "x", "o" } },
      { "il", select("@loop.inner"), mode = { "x", "o" } },
      { "uc", select("@comment.outer"), mode = { "x", "o" } },

      { "]m", move("goto_next_start", "@function.outer"), mode = { "n", "x", "o" } },
      { "]c", move("goto_next_start", "@class.outer"), mode = { "n", "x", "o" } },
      { "]M", move("goto_next_end", "@function.outer"), mode = { "n", "x", "o" } },
      { "]C", move("goto_next_end", "@class.outer"), mode = { "n", "x", "o" } },
      { "[m", move("goto_previous_start", "@function.outer"), mode = { "n", "x", "o" } },
      { "[c", move("goto_previous_start", "@class.outer"), mode = { "n", "x", "o" } },
      { "[M", move("goto_previous_end", "@function.outer"), mode = { "n", "x", "o" } },
      { "[C", move("goto_previous_end", "@class.outer"), mode = { "n", "x", "o" } },

      { "<leader>a", swap("swap_next", "@parameter.inner"), mode = "n" },
      { "<leader>A", swap("swap_previous", "@parameter.inner"), mode = "n" },
    },
  },
  {
    "nvim-treesitter/nvim-treesitter-context",
    lazy = false,
    keys = {
      {
        "<M-t>",
        function()
          require("treesitter-context").toggle()
        end,
        desc = "Toggle treesitter-context",
      },
      {
        "[x",
        function()
          require("treesitter-context").go_to_context(vim.v.count1)
        end,
        desc = "Jump to context",
      },
    },
    opts = {
      max_lines = 10,
      trim_scope = "inner",
    },
  },
}

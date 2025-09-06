return {
  "nvim-mini/mini.nvim",
  version = false,
  -- keys = {
  --   "<localleader>sa",
  --   "<localleader>sd",
  --   "<localleader>sr",
  --   "<localleader>sf",
  --   "<localleader>sF",
  --   "<localleader>sh",
  --   "<localleader>sn",
  -- },
  config = function()
    -- keys = {
    --   { "a", mode = { "x", "o" } },
    --   { "i", mode = { "x", "o" } },
    -- },
    local ai = require("mini.ai")
    ai.setup({
      n_lines = 500,
      custom_textobjects = {
        o = ai.gen_spec.treesitter({
          a = { "@block.outer", "@conditional.outer", "@loop.outer" },
          i = { "@block.inner", "@conditional.inner", "@loop.inner" },
        }, {}),
        f = ai.gen_spec.treesitter({ a = "@function.outer", i = "@function.inner" }, {}),
        c = ai.gen_spec.treesitter({ a = "@class.outer", i = "@class.inner" }, {}),
        t = { "<([%p%w]-)%f[^<%w][^<>]->.-</%1>", "^<.->().*()</[^/]->$" },
      },
    })

    require("mini.align").setup()
    require("mini.surround").setup({
      mappings = {
        add = "<localleader>sa", -- Add surrounding in Normal and Visual modes
        delete = "<localleader>sd", -- Delete surrounding
        find = "<localleader>sf", -- Find surrounding (to the right)
        find_left = "<localleader>sF", -- Find surrounding (to the left)
        highlight = "<localleader>sh", -- Highlight surrounding
        replace = "<localleader>sr", -- Replace surrounding
        update_n_lines = "<localleader>sn", -- Update `n_lines`

        suffix_last = "l", -- Suffix to search with "prev" method
        suffix_next = "n", -- Suffix to search with "next" method
      },
    })

    local mi = require("mini.icons")
    mi.setup()
    mi.mock_nvim_web_devicons()

    local hipatterns = require("mini.hipatterns")
    hipatterns.setup({
      highlighters = {
        hex_color = hipatterns.gen_highlighter.hex_color(),
      },
    })
  end,
}

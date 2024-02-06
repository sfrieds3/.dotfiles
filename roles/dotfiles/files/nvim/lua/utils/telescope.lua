local Utils = require("utils.utils")

local M = setmetatable({}, {
  __call = function(m, picker, opts)
    local map = setmetatable({
      custom = require("plugins.telescope.custom"),
      undo = require("telescope").extensions.undo,
    }, {
      __index = function()
        return require("telescope.builtin")
      end,
    })

    m.telescope(map[opts.type], picker, opts)
  end,
})

M.theme = setmetatable({
  dropdown = "get_dropdown",
  ivy = "get_ivy",
  cursor = "get_cursor",
}, {
  __index = function()
    return nil
  end,
})

function M.telescope(type, picker, opts)
  opts = opts or { "previewer = false" }
  local theme = M.theme[Utils.remove_key(opts, "theme") or ""]

  if theme then
    return type[picker](require("telescope.themes")[theme](opts))
  else
    return type[picker](opts)
  end
end

return M

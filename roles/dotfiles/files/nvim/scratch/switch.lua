local M = setmetatable({}, {
  __call = function(m, type, ...)
    local function switch(choice)
      case = {
        custom = "hello",
        builtin = "test",
      }
      return case[choice]
    end
    P("in switch")
    return switch(type)
  end,
})

P(M("custom"))

function test_function()
  local t = { text = "this is text", second = 5 }
  return t
end

local query_text = [[
(function_declaration
  name: (identifier) @function)
  ]]

local table_query_text = [[
(table_constructor
  (field
    name: (identifier) @field_name
    value: (string
    content: (string_content) @field_value)))
  ]]

local lang = "lua"
local parser = vim.treesitter.get_parser(0, lang)

local tree = parser ~= nil and parser:parse()
local root = tree[1]:root()
local query = vim.treesitter.query.parse("lua", table_query_text)

-- local source = "function test_function() return { text = 'this is text', second = 5 } end"
-- local tree = vim.treesitter.get_string_parser(source, "lua"):parse()
-- local root = tree[1]:root()

local nodes = {}
for id, node in query:iter_captures(root, 0) do
  local name = query.captures[id]
  local type = node:type()
  table.insert(nodes, { node = node, name = name, type = type })
end

local s = {}
for _, node in ipairs(nodes) do
  table.insert(s, node.type)
  table.insert(s, vim.treesitter.get_node_text(node.node, 0))
end

P(s)

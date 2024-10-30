local function test_co()
  for i = 1, 10 do
    coroutine.yield(i)
  end
end

local f = coroutine.wrap(test_co)

for i = 1, 10 do
  print(f())
end

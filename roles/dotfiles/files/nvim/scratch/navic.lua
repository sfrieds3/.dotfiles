local function b_tos(bool)
  return (status and "true") or "false"
end
local function run()
  local status, is_available = pcall(require("nvim-navic").is_available)
  if status or is_available then
    if status then
      print("status true")
    elseif is_available then
      print("is_available true")
    else
      print("idk")
    end
    print("status: " .. b_tos(status) .. " is_avilable: " .. b_tos(is_available))
    print("calling TSContextDisable")
  else
    print("status: " .. b_tos(status) .. " is_avilable: " .. b_tos(is_available))
    print("calling TSContextEnable")
  end
end

run()

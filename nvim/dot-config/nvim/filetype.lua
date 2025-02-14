local function is_helm_file(path)
  local check = vim.fs.find("Chart.yaml", { path = vim.fs.dirname(path), upward = true })
  return not vim.tbl_isempty(check)
end

--@private
--@return string
local function yaml_filetype(path, bufname)
  return is_helm_file(path) and "helm" or "yaml"
end

--@private
--@return string
local function tmpl_filetype(path, bufname)
  return is_helm_file(path) and "helm" or "template"
end

--@private
--@return string
local function tpl_filetype(path, bufname)
  return is_helm_file(path) and "helm" or "jinja"
end

vim.filetype.add({
  extension = {
    yaml = yaml_filetype,
    yml = yaml_filetype,
    tmpl = tmpl_filetype,
    tpl = tpl_filetype,
    zsh = "zsh",
    qvs = "qvs",
    tmplt = "jinja",
    jinja = "jinja",
    jinja2 = "jinja",
    j2 = "jinja",
  },
  filename = {
    ["Chart.yaml"] = "yaml",
    ["Chart.lock"] = "yaml",
  },
  pattern = {
    -- markdown
    ["TODO"] = "markdown",
    -- zsh
    ["${XDG_CONFIG_HOME}/git"] = "git",
    ["${XDG_CONFIG_HOME}/zsh"] = "zsh",
    ["*zprofile"] = "zsh",
    ["*zshenv"] = "zsh",
    ["*zshrc"] = "zsh",
    -- docker
    ["Dockerfile.*"] = "dockerfile",
    -- ansible
    [".*/host_vars/.*%.ya?ml"] = "yaml.ansible",
    [".*/group_vars/.*%.ya?ml"] = "yaml.ansible",
    [".*/group_vars/.*/.*%.ya?ml"] = "yaml.ansible",
    [".*/playbooks/.*%.ya?ml"] = "yaml.ansible",
    [".*/roles/.*/tasks/.*%.ya?ml"] = "yaml.ansible",
    [".*/roles/.*/handlers/.*%.ya?ml"] = "yaml.ansible",
    ["${HOME}/.dotfiles/*.ya?ml$"] = function(path, bufnr)
      local content = vim.api.nvim_buf_get_lines(bufnr, 0, 1, false)[1] or ""
      if vim.regex([[hosts:\|tasks:]]):match_str(content) then
        return "yaml.ansible"
      end
    end,
  },
})

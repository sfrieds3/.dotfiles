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
    -- j2 = "jinja",
    mdx = "markdown",
  },
  filename = {
    ["Chart.yaml"] = "yaml",
    ["Chart.lock"] = "yaml",
  },
  pattern = {
    -- markdown
    ["TODO"] = "markdown",
    -- git
    [".*gitconfig"] = "gitconfig",
    -- zsh
    ["${XDG_CONFIG_HOME}/git"] = "git",
    ["${XDG_CONFIG_HOME}/zsh"] = "zsh",
    ["*zprofile"] = "zsh",
    ["*zshenv"] = "zsh",
    ["*zshrc"] = "zsh",
    -- docker
    ["Dockerfile.*"] = "dockerfile",
    -- k8s/helm
    [".*.json.gotmpl"] = "json",
    [".*.md.gotmpl"] = "markdown",
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
    [".*"] = {
      function(path, bufnr)
        local line = vim.api.nvim_buf_get_lines(bufnr, 0, 1, false)[1] or ""
        line = line:match("^%s*(.-)%s*$") -- trim

        -- JSON
        if line:match("^%{") or line:match("^%[") then
          return "json"
        end

        -- YAML
        if line:match("^%-%-%-") or line:match("^%w+:%s") then
          return "yaml"
        end

        -- TOML
        if line:match("^%w+%s-=%s-[\"']?") then
          return "toml"
        end

        -- CSV
        if line:match("^[^,]+,[^,]+") then
          return "csv"
        end

        -- INI
        if line:match("^%[.-%]") or line:match("^%w+%s-=%s-") then
          return "dosini"
        end

        -- .env / dotenv
        if line:match("^%w[%w_]*=") then
          return "sh" -- or 'dosini', depending on your preference
        end

        -- Dockerfile
        if line:match("^FROM ") or line:match("^RUN ") or line:match("^CMD ") then
          return "dockerfile"
        end

        -- XML
        if line:match("^<%?xml") or line:match("^<[%w_]+") then
          return "xml"
        end

        -- HTML
        if line:match("^<!DOCTYPE html>") or line:match("^<html>") then
          return "html"
        end

        -- Markdown
        if line:match("^#") or line:match("^%* ") or line:match("^```") then
          return "markdown"
        end

        -- SQL
        if line:match("^%s*SELECT ") or line:match("^%s*CREATE ") or line:match("^%s*INSERT ") then
          return "sql"
        end

        -- ReStructuredText
        if line:match("^==+$") or line:match("^%-%-+$") or line:match("^%.%. ") then
          return "rst"
        end

        -- fallback to default filename detection
        return vim.filetype.match({ filename = path })
      end,
      { priority = -math.huge },
    },
  },
})

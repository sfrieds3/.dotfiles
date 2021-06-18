local snap = require('snap')

-- live rg
snap.register.map({'n'}, {'\\fz'}, function()
  snap.run {
    producer = snap.get'producer.ripgrep.vimgrep',
    select = snap.get'select.vimgrep'.select,
    multiselect = snap.get'select.vimgrep'.multiselect,
    views = {snap.get'preview.vimgrep'}
  }
end)

-- vimgrep, <C-q> run fzf on filtered results
snap.register.map({'n'}, {'\\fZ'}, function()
snap.run {
  producer = snap.get'producer.ripgrep.vimgrep',
  steps = {{
    consumer = snap.get'consumer.fzf',
    config = {prompt = "FZF>"}
  }},
  select = snap.get'select.file'.select,
  multiselect = snap.get'select.file'.multiselect,
  views = {snap.get'preview.file'}
}
end)

-- find files
snap.register.map({'n'}, {'\\fl'}, function()
  snap.run {
  producer = snap.get'consumer.fzf'(snap.get'producer.ripgrep.file'),
  select = snap.get'select.file'.select,
  multiselect = snap.get'select.file'.multiselect,
  views = {snap.get'preview.file'}
}
end)

-- find git files
snap.register.map({'n'}, {'\\fg'}, function()
  snap.run {
  producer = snap.get'consumer.fzf'(snap.get'producer.git.file'),
  select = snap.get'select.file'.select,
  multiselect = snap.get'select.file'.multiselect,
  views = {snap.get'preview.file'}
}
end)

snap.register.map({'n'}, {'\\fo'}, function()
  snap.run {
    producer = snap.get'consumer.fzf'(snap.get'producer.vim.oldfile'),
    select = snap.get'select.file'.select,
    multiselect = snap.get'select.file'.multiselect,
    views = {snap.get'preview.file'}
  }
end)

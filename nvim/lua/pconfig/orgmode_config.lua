local sfmt = string.format
local code_dir = "~/src"
local org_dir = string.format("%s/org", code_dir)

require('orgmode').setup({
  org_agenda_files = { sfmt("%s/*", org_dir), sfmt("%s/**/*", org_dir) },
  org_default_notes_file = sfmt("%s/refile.org", org_dir),
  org_todo_keywords = {'TODO', 'WORK', 'WAIT', '|', 'DONE', 'CNCL'}
})

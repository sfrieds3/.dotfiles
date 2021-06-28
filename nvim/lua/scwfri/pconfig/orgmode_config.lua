require('orgmode').setup({
  org_agenda_files = {'~/code/org/*', '~/code/org/**/*'},
  org_default_notes_file = '~/code/org/refile.org',
  org_todo_keywords = {'TODO', 'WORK', 'WAIT', '|', 'DONE', 'CNCL'}
})

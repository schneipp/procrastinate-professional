" procrastinate.vim - Neovim client for Procrastinate-Professional
" Maintainer: Your Name
" Version: 1.0.0

if exists('g:loaded_procrastinate')
  finish
endif
let g:loaded_procrastinate = 1

" Commands
command! Procrastinate lua require('procrastinate').open()
command! ProcrastinateRefresh lua require('procrastinate').refresh()
command! ProcrastinateAdd lua require('procrastinate').add_todo()

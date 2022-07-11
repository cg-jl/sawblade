

if exists("g:syntax")
  finish
endif

let g:syntax = 'sawblade'

syn keyword sawbladeKw block add
syn keyword sawbladeSpecKw return arguments


hi def link sawbladeKw Keyword

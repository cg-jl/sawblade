

if exists("g:syntax")
  finish
endif

let g:syntax = 'abism'

syn keyword abismKw block add
syn keyword abismSpecKw return arguments


hi def link abismKw Keyword

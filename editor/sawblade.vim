

if exists("g:syntax")
  finish
endif

let g:syntax = 'sawblade'
set iskeyword+='-'

syn keyword sawbladeKw block return arguments
syn keyword sawbladeInsn add call sub flags br
syn region sawbladeExportLabel start=/\v"/ end=/\v"/
syn match sawbladeColonColon /::/
syn match sawbladeRefLabel /@\w\+/
syn match sawbladeLocal /%\w\+/
syn match sawbladeNum /\d\+/


hi def link sawbladeKw @keyword
hi def link sawbladeInsn @function.builtin
hi def link sawbladeExportLabel @string
hi def link sawbladeRefLabel @function.call
hi def link sawbladeLocal @symbol
hi def link sawbladeNum @number

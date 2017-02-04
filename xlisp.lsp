; xlisp.lsp - initialization code for XLisp version 3.0

; load the xlisp support code
(load "xlinit.lsp")

; load any user extensions
(load "local.ini")

; load the files mentioned on the command line
(loader 1)


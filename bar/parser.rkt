#lang ragg
program : sexpr* start* layout
sexpr   : SEXPR

; note: the first WORD must be "start"
start   : WORD WORD "[" WORD* "=" ">" sexpr "]"

layout  : elem*
elem    : info | text | sexpr
info    : "{" text "}" ; TODO more structure
text    : WORD*

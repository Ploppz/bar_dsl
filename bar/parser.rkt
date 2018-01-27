#lang racket
(require megaparsack megaparsack/text)
(require parser-tools/lex) ; for position-token
(require megaparsack/parser-tools/lex) ; for token/p
(require data/monad) ; for do
(require data/applicative) ; for pure

(provide bar/p)
(define bar/p (do (many/p (token/p 'SEXPR)) (many/p start/p) layout/p))
(define start/p (do
                  (token/p 'WORD)
                  (token/p 'WORD)
                  (token/p '\[ )
                  (many/p (token/p 'WORD))
                  (token/p '\= )
                  (token/p '\> )
                  (token/p 'SEXPR)
                  (token/p '\] )))
(define layout/p (token/p '\. ))


; Old parser in ragg:

; program : sexpr* start* layout
; sexpr   : SEXPR

; ; note: the first WORD must be "start"
; start   : WORD WORD "[" WORD* "=" ">" sexpr "]"

; layout  : elem*
; elem    : info | text | sexpr
; info    : "{" text "}" ; TODO more structure
; text    : WORD*

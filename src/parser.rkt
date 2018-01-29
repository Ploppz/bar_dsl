#lang racket
(provide bar/p)
(require "lex.rkt"
         megaparsack
         data/monad ; for do
         data/applicative) ; for pure

(define sexpr/p
  (do [val <- (token/p 'SEXPR)] (pure (sexpr/code val))))

(define bar/p (do
                [inits <- (many/p sexpr/p)]
                [starts <- (many/p start/p)]
                layout/p
                (pure (bar/code inits starts))))
(define start/p (do
                  (token/p 'WORD "start")
                  [start-name <- (token/p 'WORD)]
                  (token/p 'CHAR "[")
                  [params <- (many/p (token/p 'WORD))]
                  (token/p 'CHAR "=")
                  (token/p 'CHAR ">")
                  [transform <- sexpr/p]
                  (token/p 'CHAR "]")
                  (pure (start/code start-name params transform))))

(define layout/p (token/p 'CHAR "." ))


;;; Translation from syntax-box'es to code.
(define (bar/code inits starts) 0) ; TODO
(define (start/code start-name params transform)
  `(thread (#,start-name (lambda (#,@params) #,transform))))

(define (sexpr/code tok)
  (match tok
    [(token name val)
     (read (open-input-string val))]))


; Old parser in ragg:

; program : sexpr* start* layout
; sexpr   : SEXPR

; ; note: the first WORD must be "start"
; start   : WORD WORD "[" WORD* "=" ">" sexpr "]"

; layout  : elem*
; elem    : info | text | sexpr
; info    : "{" text "}" ; TODO more structure
; text    : WORD*

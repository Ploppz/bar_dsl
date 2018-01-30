#lang racket
; This file creates a tokenizer using `lexer`.
(provide make-tokenizer)

(require data/applicative               ; pure
         data/monad)                    ; do

(require "lex.rkt")

(define (make-tokenizer port)
  (lexer port
    ; Implicit eof test
    [(char=? lexer/c #\() (token/s-expression lexer/port) 'SEXPR]
    [(char=? lexer/c #\\) (token/chars lexer/port 2) 'ESCAPED-CHAR]
    [(char-word? lexer/c) (token/word lexer/port) 'WORD]
    [else                 (token/chars lexer/port 1) 'CHAR]))

(define (token/chars port n)
  (read-string n port)
  n)

(define (token/s-expression port)
  (let ([left #\(] [right #\)])
    (define (balance level)
      (define c (read-char port))
      (if (eof-object? c)
          0
          (cond
                [(char=? c left) (+ 1 (balance (+ level 1)))]
                [(char=? c right) (if (= level 1) 0 (+ 1 (balance (- level 1))))]
                [else (+ 1 (balance level))])))
    (+ 1 (balance 0))))

(define (char-word? c)
  (or (char-alphabetic? c)
      (char-numeric? c)
      (char=? c #\-)
      (char=? c #\-)))

(define (token/word port)
  (define c (peek-char port))
  (cond 
    [(eof-object? c) 0]
    [(char-word? c) (read-char port) (+ 1 (token/word port))]
    [else 0]))

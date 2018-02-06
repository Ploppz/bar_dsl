#lang racket/base
; This file creates a tokenizer using `lexer`, and provides `read-syntax`.
(provide read-syntax
         make-tokenizer)

(require data/applicative              ; pure
         data/monad                    ; do
         megaparsack
         "lex.rkt"
         "parser.rkt")

(define (read-syntax path port)
  (define (reduce gen)
    (define token (gen))
    (if (not token)
      (list)
      (cons token (reduce gen))))
  (define next-token (make-tokenizer port))
  (define tokens (reduce next-token))
  (define dat (parse-result! (parse bar/p tokens)))
  (datum->syntax #f dat))


(define (make-tokenizer port)
  (lexer port
    ; Implicit eof test
    [(char=? lexer/c #\;) (token/comment lexer/port) #f] ; no token
    [(char=? lexer/c #\() (token/s-expression lexer/port) 'SEXPR]
    [(char-word? lexer/c) (token/word lexer/port) 'WORD]
    [else                 (token/chars lexer/port 1) 'CHAR]))

(define (token/comment port)
  (string-length (read-line port)))
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
      (char=? c #\_)))

(define (token/word port)
  (define c (peek-char port))
  (cond 
    [(eof-object? c) 0]
    [(char-word? c) (read-char port) (+ 1 (token/word port))]
    [else 0]))

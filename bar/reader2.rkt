#lang racket
(require megaparsack megaparsack/text)
(require parser-tools/lex)              ; position-token
(require megaparsack/parser-tools/lex)  ; token/p
(require data/monad)                    ; do
(require data/applicative)              ; pure

(require (for-syntax syntax/parse))
; (lexer port (predicate parser) ...)
; `parser`s should simply read port until end of lexeme.
; If the `parser` is called, this macro will return (position-token a)
(define-syntax (lexer stx)
  (syntax-parse stx
    ; Entry point for the syntax which should be used
    [(_ port (pred parser token) ...)
      #'(begin
          (port-count-lines! port)
          (define (next-token)
            (define c (peek-char port))
            (cond
              [(eof-object? c) #f]
              [(char-whitespace? c) (read-char port) (next-token)]
              (lexer helper port (pred parser token) ...))
            (next-token))
          next-token
      )
     ]

    ; Base case:
    [(_ (~literal helper) port)
     ; end of cond (without else)
     #'(void)]
    ; Helpers for recursion - what better way to do this?
    [(_ (~literal helper) port (pred parser token) (x y z) ...)
      #'(begin
          [(pred c)
          (define-values (start-line start-col start-pos) (port-next-location port))
          (void (parser))
          (define-values (end-line end-col end-pos) (port-next-location port))
          (position-token token (position start-pos start-line start-col) (position end-pos end-line end-col)) ]

          (lexer helper port (x y z) ...))]
    [(_ (~literal helper) port ((~literal else) parser token))
      #'(begin
          [else
          (define-values (start-line start-col start-pos) (port-next-location port))
          (void (parser))
          (define-values (end-line end-col end-pos) (port-next-location port))
          (position-token token (position start-pos start-line start-col) (position end-pos end-line end-col)) ]
          ; end of cond
          )]))

(define (make-tokenizer port)
  (lexer port
    [(lambda (c) char=? c #\() (lambda (port) (balance-brackets port #\( #\))) 'SEXPR]
    [char-word? parse-word 'WORD]
    [else (lambda (port) (read-string 1 port)) 'CHAR] ; TODO: Add else to macro!
    ))


#|
(define (make-tokenizer port)
  (port-count-lines! port)
  (define (next-token)
    (define c (peek-char port))
    (cond
          [(eof-object? c) #f]
          [(char=? c #\() (call-with-values
                            (lambda () (balance-brackets port #\( #\)))
                            (lambda (start end) (position-token 'SEXPR start end)))] ; S-expression
          [(char-word? c) (call-with-values
                            (lambda () (parse-word port))
                            (lambda (start end) (position-token 'WORD start end)))] ; word
          
          [(char-whitespace? c) (read-char port) (next-token)]
          [else (token c (read-string 1 port) )]))
  next-token)
(provide make-tokenizer)
|#

(define (balance-brackets port left right)
  ; TODO: get indices instead of string-append.
  (define (balance level)
    (displayln level)
    (define c (read-char port))
    (if (eof-object? c)
        0
        (cond
              [(char=? c left) (displayln "left") (balance (+ level 1))]
              [(char=? c right) (displayln "right") (if (= level 1) 0 (balance (- level 1)))]
              [else (balance level)])))
  ; Find the start location and end location of the substring we are searching for
  (define-values (start-line start-col start-pos) (port-next-location port))
  (void (balance 0))
  (define-values (end-line end-col end-pos) (port-next-location port))
  (values (position start-pos start-line start-col) (position end-pos end-line end-col)))
(provide balance-brackets)

(define (char-word? c)
  (or (char-alphabetic? c)
      (char-numeric? c)
      (char=? c #\-)
      (char=? c #\-)))

; Returns the parsed word
(define (parse-word port)
  (define (parse)
    (define c (peek-char port))
    (cond 
      [(eof-object? c) 0]
      [(char-word? c) (read-char port) (parse)]
      [else 0]))
  (define-values (start-line start-col start-pos) (port-next-location port))
  (void (parse))
  (define-values (end-line end-col end-pos) (port-next-location port))
  (values (position start-pos start-line start-col) (position end-pos end-line end-col)))
(provide parse-word)



;;;;; Testing
(define (reduce gen)
  (define token (gen))
  (if (false? token)
    (list)
    (cons token (reduce gen))))
(provide reduce)

#lang racket
(require megaparsack megaparsack/text)
(require data/applicative)              ; pure
(require parser-tools/lex)              ; position-token
(require megaparsack/parser-tools/lex)  ; token/p
(require data/monad)                    ; do
(require (for-syntax syntax/parse))     ; syntax-parse
(require racket/stxparam)

(provide make-tokenizer)

(define-syntax-parameter lexer/c
  (lambda (stx)
    (raise-syntax-error (syntax-e stx) "undefined or outside of `lexer`.")))

(begin-for-syntax
  (define (transform port clause)
    (with-syntax ([(pred parser token) clause])
                 ; Make a clause of a cond
                 #`[pred
                    (define-values (start-line start-col start-pos) (port-next-location #,port))
                    (void parser)
                    (define-values (end-line end-col end-pos) (port-next-location #,port))
                    (position-token token (position start-pos start-line start-col)
                                    (position end-pos end-line end-col)) 
                    ])))

; (lexer port (predicate parser) ...)
; `parser`s should simply read port until end of lexeme.
; If the `parser` is called, this macro will return (position-token a)
(define-syntax (lexer stx)
  (syntax-parse stx
    ; Entry point for the syntax which should be used
    [(_ port (pred parser token) ...)
     ; just sending `port` to the transform function
     (define transformer (lambda (clause) (transform #'port clause))) 
     ; making a list from the ...
     (define input-clauses (syntax-e #'((pred parser token) ...))) 
     (define clauses (map transformer input-clauses))
      #`(begin
          (port-count-lines! port)
          (define (next-token)
            (define c (peek-char port))
            (syntax-parameterize ([lexer/c (make-rename-transformer #'c)])
              (cond
                [(eof-object? c) #f]
                [(char-whitespace? c) (read-char port) (next-token)]
                #,@clauses
               )))
          next-token)
      ]))

(define (make-tokenizer port)
  (lexer port
    ; Implicit eof and whitespace test
    [(char=? lexer/c #\() (token/s-expression port) 'SEXPR]
    [(char-word? lexer/c) (token/word port) 'WORD]
    [else (read-string 1 port) (string->symbol (string lexer/c))]
    ))


(define (token/s-expression port)
  (let ([left #\(] [right #\)])
    (define (balance level)
      (displayln level)
      (define c (read-char port))
      (if (eof-object? c)
          0
          (cond
                [(char=? c left) (displayln "left") (balance (+ level 1))]
                [(char=? c right) (displayln "right") (if (= level 1) 0 (balance (- level 1)))]
                [else (balance level)])))
    (balance 0)))

(define (char-word? c)
  (or (char-alphabetic? c)
      (char-numeric? c)
      (char=? c #\-)
      (char=? c #\-)))

(define (token/word port)
  (define c (peek-char port))
  (cond 
    [(eof-object? c) 0]
    [(char-word? c) (read-char port) (token/word port)]
    [else 0]))

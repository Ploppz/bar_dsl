#lang racket
(provide lexer          ; Macro to create a lexer
         lexer/c        ; Current character syntax parameter of lexer.
         lexer/port     ; The port which parsers should read from.
         (struct-out token)
         token-type/p
         token/p)

(require (for-syntax syntax/parse)     ; syntax-parse
          (for-syntax megaparsack)
          racket/stxparam
          megaparsack
          racket/match
          data/applicative              ; pure
          data/monad)                   ; do

(struct token (name value) #:transparent)
;;;;;;;;;;;
;;; Parsers
;        token-type/p: parse a token with name `name`, and optionally value `value`.
(define (token-type/p name [value #f])
  (define (name-equal? tok)
    (match tok
           [(token tok-name tok-value)
            (and (equal? tok-name name) (nand value (not (equal? tok-value value))))]
           [else #f]))
  (label/p
    (symbol->string name)
    (do [tok <- (satisfy/p name-equal?)]
      (pure tok))))

;        token/p: parse a token value `value`.
(define (token/p value)
  (define (val-equal? tok)
    (match tok
           [(token _ tok-value) (equal? tok-value value)]
           [else #f]))
  (label/p
    value
    (do [tok <- (satisfy/p val-equal?)]
      (pure tok))))

;;;;;;;;;;;;;;;;;
;;; Macro `lexer`
(define-syntax-parameter lexer/c
  (lambda (stx)
    (raise-syntax-error (syntax-e stx) "undefined or outside of `lexer`.")))
(define-syntax-parameter lexer/port
  (lambda (stx)
    (raise-syntax-error (syntax-e stx) "undefined or outside of `lexer`.")))

(begin-for-syntax
  (define (transform port clause)
    (with-syntax ([(pred parser name) clause])
                 ; Make a clause of a cond
                 #`[pred
                    ; This is ugly:
                    ; Make a peeking port `port2` which we give to the parser.
                    ; Then read the string from `port`.
                    (define-values (start-line start-col start-pos) (port-next-location #,port))
                    (define port2 (peeking-input-port #,port))
                    (syntax-parameterize ([lexer/port (make-rename-transformer #'port2)])
                      (define span parser)
                      ; ^ TODO: not really necessary that the parsers return span.
                      ;         We can get from port-next-location.
                      (define value (read-string span #,port))
                      (create-token name value "undefined" start-line start-col start-pos span))
                    ])))
(define (create-token name value srcname line column position span)
  (syntax-box (token name value) (srcloc srcname line column position span)))

; (lexer port (predicate parser) ...)
; `parser`s should simply read port until end of lexeme, and return the number of characters read.
(define-syntax (lexer stx)
  (syntax-parse stx
    ; Entry point for the syntax which should be used
    [(_ port (pred parser token) ...)
     (define transformer (lambda (clause) (transform #'port clause))) 
     (define input-clauses (syntax-e #'((pred parser token) ...))) ; making a list from the ...
     (define clauses (map transformer input-clauses))
      #`(begin
          (port-count-lines! port)
          (define (next-token)
            (define c (peek-char port))
            (syntax-parameterize ([lexer/c (make-rename-transformer #'c)])
              (cond
                [(eof-object? c) #f]
                #,@clauses)))
          next-token) ]))

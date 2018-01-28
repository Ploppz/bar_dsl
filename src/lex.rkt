#lang racket
(provide lexer          ; Macro to create a lexer
         lexer/c)        ; Current character syntax parameter of lexer.
; Use megaparsack's `parse` to parse the resulting tokens.

(require (for-syntax syntax/parse)     ; syntax-parse
          (for-syntax megaparsack)
          racket/stxparam
          megaparsack
          racket/match
          data/applicative              ; pure
          data/monad)                   ; do

;;; TOKENS
(define (token? v)
  (match v [(syntax-box tok _)   (symbol? tok)]
           [else        #f]))
(define (token-value tok)
  (match tok
         [(syntax-box tok _)   (if (symbol? tok) tok #f)]
         [else      #f])) ; TODO: Appropriate way to cast error here?

;;; PARSER
(define (token/p name)
(label/p
   (symbol->string name)
   (do [tok <- (satisfy/p token?)]
     (pure (token-value tok)))))

;;; Macro `lexer`
(define-syntax-parameter lexer/c
  (lambda (stx)
    (raise-syntax-error (syntax-e stx) "undefined or outside of `lexer`.")))
(begin-for-syntax
  (define (transform port clause)
    (with-syntax ([(pred parser token) clause])
                 ; Make a clause of a cond
                 #`[pred
                    (define-values (start-line start-col start-pos) (port-next-location #,port))
                    (define span (parser))
                    (create-token token "undefined" start-line start-col start-pos span)
                    ])))
(define (create-token token srcname line column position span)
  (syntax-box token (srcloc srcname line column position)))

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
                [(char-whitespace? c) (read-char port) (next-token)]
                #,@clauses)))
          next-token) ]))

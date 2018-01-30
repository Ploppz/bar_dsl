#lang racket
(provide bar/p)
(require "lex.rkt"
         megaparsack
         data/monad ; for do
         data/applicative) ; for pure

; Parser summary:

; program : sexpr* start* layout
; sexpr   : SEXPR

; start   : "start" WORD "[" WORD* "=" ">" sexpr "]"

; TODO
; OMG.. in the following, we have to deal with spaces indeed..
; layout  : (orientation elem*)+
; orientation : ("@left" | "@right" | "@center")
; elem    : info | text | sexpr
; widget  : "#" WORD
; text    : (WORD | CHAR)*

;;;;;;;;;;;
;;; PARSERS
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

;;;;;;;;;;;;;;;;;;;;
;;; CODE TRANSLATION

(define (bar/code inits starts)
  (define start-codes (map (lambda (x) (car x)) starts))
  (define match-codes (map (lambda (x) (cdr x)) starts))
  ; ^ TODO: somehow one parenthesis too many
  `(
    (require racket/serialize)
    ,@inits
    (define-values (pipe-in) (pipe-out) (make-pipe))
    ,@start-codes

    (define (loop)
      (define obj (deserialize pipe-out))
      (match obj ,@match-codes)
      ; ^ TODO: Not that simple. Need to keep state about each widget..
      ;     That is... store the `-state` obj and update it.
      ; Plan:
      ;   - Define the state variables first.
      ;   - Here in the loop, we need to go through the format! When e.g. {mpd} is reached,
      ;     just apply the (match-) transform to the saved state variable.
      (loop))

    ))

(define (start/code start-name params transform)
  (define name (token-value start-name))
  (define listener-name (string->symbol (format "~a-listener" name)))
  (define state-name (string->symbol (format "~a-state" name)))
  (list 
    `(thread (,listener-name pipe-in))          ; Start thread
    `[(,state-name ,@(map token->ident params)) ; Match clause
      ,transform]))

(define (sexpr/code tok)
  (match tok
    [(token name val)
     (read (open-input-string val))]))

(define (token->ident tok)
  (match tok [(token 'WORD val)
              (string->symbol val)]))



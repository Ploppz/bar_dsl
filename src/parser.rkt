#lang racket
(provide bar/p)
(require "lex.rkt"
         megaparsack
         data/monad ; for do
         data/applicative) ; for pure

;;;;;;;;;;;
;;; PARSING

;; program : SEXPR* start* layout+
(define sexpr/p
  (do [val <- (token-type/p 'SEXPR)] (pure (sexpr/code val))))

(define bar/p (do
                [inits <- (many/p (do sexpr/p spaces/p))]
                [starts <- (many/p start/p (token/p "\n"))]
                [layouts <- (many/p layout/p #:min 1 #:max 3)]
                (pure (bar/code inits starts))))
;; start   : "start" WORD "[" WORD* "=" ">" sexpr "]"
(define start/p (do
                  (token/p "start")
                  [start-name <- (token-type/p 'WORD)]
                  (token/p "[")
                  [params <- (many/p (token-type/p 'WORD))]
                  (token/p "=")
                  (token/p ">")
                  [transform <- sexpr/p]
                  (token/p "]")
                  (token/p "\n")
                  (pure (start/code start-name params transform))))

;; layout  : (orientation elem*)+
(define layout/p (many/p (do orientation/p (many/p element/p)) #:min 1))
; orientation : ("@left" | "@right" | "@center")
(define orientation/p (or/p (do (token/p "@") (token/p "left") (pure 'left))
                            (do (token/p "@") (token/p "right") (pure 'right))
                            (do (token/p "@") (token/p "center") (pure 'center))))
;; elem    : info | text | sexpr
(define element/p (or widget/p text/p sexpr/p))
;; widget  : "#" WORD
(define widget/p (do (token/p "#") (token-type/p 'WORD)))
;; text    : (WORD | CHAR)*
(define text/p (or (token-type/p 'WORD) (token-type/p 'CHAR)))

(define spaces/p (many/p (hidden/p space/p)))
(define space/p (or/p (token/p "\n") (token/p " ") (token/p "\t")))

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



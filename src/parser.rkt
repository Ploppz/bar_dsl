#lang racket
(provide bar/p)
(require "lex.rkt"
         megaparsack
         data/monad ; for do
         data/applicative) ; for pure

;;;;;;;;;;;
;;; PARSING

;;; Helpers
(define (text-char? tok)
  (match tok
         [(token 'CHAR val) (nor (string=? val "(") (string=? val ")") (string=? val "{") (string=? val "}") (string=? val "\n"))]))
(struct widget (name))
;;;

(define sexpr/p
  (do [val <- (token-type/p 'SEXPR)] (pure (sexpr/code val))))

;; program : SEXPR* start* layout+
(define bar/p (do
                [inits <- (many/p (do sexpr/p spaces/p))]
                [starts <- (many/p start/p)]
                [layout <- layout/p]
                eof/p
                (pure (bar/code inits starts layout))))
;; start   : "start" WORD "[" WORD* "=" ">" sexpr "]"
(define start/p (do
                  (token/p "start") spaces/p
                  [start-name <- (token-type/p 'WORD)] spaces/p
                  (token/p "[") spaces/p
                  [params <- (many/p (do [param <- (token-type/p 'WORD)] spaces/p (pure param)))] 
                  (token/p "=") (token/p ">") spaces/p
                  [transform <- sexpr/p] spaces/p
                  (token/p "]") spaces/p
                  (pure (start/code start-name params transform))))

;; text    : WORD
; Returns a string (possibly part of a bigger)
(define text/p (do [text-tokens <- (or/p
                                         (token-type/p 'WORD)
                                         (token-type/p 'ESCAPED-CHAR)
                                         (satisfy/p text-char?))]
                   (pure (token-value text-tokens))))
;; widget  : "#" WORD
(define widget/p (do (token/p "{")
                     [name <- (token-type/p 'WORD)]
                     (token/p "}")
                     space/p
                     (pure (widget name))))
;; elem    : info | text | sexpr
(define element/p (or/p widget/p text/p sexpr/p))
;; orientation : ("@left" | "@right" | "@center")
(define orientation/p (or/p (do (token/p "@") (token/p "left") (pure 'left))
                            (do (token/p "@") (token/p "right") (pure 'right))
                            (do (token/p "@") (token/p "center") (pure 'center))))
;; layout  : (orientation elem*)+
; Returns a nested list which can safely be flattened
(define layout/p (many/p (do [ori <- orientation/p]
                             spaces/p
                             [elems <- (many/p element/p)]
                             spaces/p
                             (pure (list ori elems))) #:min 1 #:max 3))

(define space/p (or/p (token/p "\n") (token/p " ") (token/p "\t")))
(define spaces/p (many/p (hidden/p space/p)))


;;;;;;;;;;;;;;;;;;;;
;;; CODE TRANSLATION

(define (bar/code inits starts layout)
  (define start-codes (map (lambda (x) (car x)) starts))
  (define match-codes (map (lambda (x) (cdr x)) starts))
  ; ^ TODO: somehow one parenthesis too many
  `(
    (require racket/serialize)
    ,@inits
    (define-values (pipe-in) (pipe-out) (make-pipe))
    ,@start-codes

    ,@layout
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



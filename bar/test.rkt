#lang racket
;(require "reader2.rkt")
(require parser-tools/lex)              ; position-token


;(define port (open-input-string "(hel(lo))help)oh"))
;(call-with-values (lambda () (balance-brackets port #\( #\))) (lambda (a b) (position-token 'X a b)))

(require (for-syntax syntax/parse))
(define-syntax (bar stx)
  (syntax-parse stx
    [(_ z) #'(void)]
    [(_ z (a:expr b:expr) (x:expr y:expr) ...) #'(begin (printf "~a:~a (~a)\n" a b z) (bar z (x y) ...))]
    ))
(bar 0 [1 2] [3 4] [5 6])

(define-syntax (foo stx)
  (syntax-parse stx
    [(_ (~literal z)) #'(printf "hello")]))
(foo z)


(define-syntax (baz stx) (syntax-parse stx [(_ code) #'(lambda (c) code)]))

;(define-syntax-rule (bar (a b)) (+ a b))
;(bar (0 2))

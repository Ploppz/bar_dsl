#lang racket
; Widgets
(define-syntax-rule (provide-from module-path)
  (begin (require module-path)
    (provide (all-from-out module-path))))
(provide-from "widget/mpd.rkt")
(provide-from "widget/clock.rkt")

; Necessities
(provide (all-from-out racket))

; Expanders
(provide start-periodic-loop)
(require (for-syntax racket/syntax syntax/parse))
(require racket/serialize)
(define-syntax (start-periodic-loop stx)
  (syntax-parse stx
    [(_ port (name period) ...)
     #:with [name-state ...] (map (lambda (name-stx) (format-id name-stx "~a-value" name-stx #:source name-stx #:props name-stx))
                                  (attribute name))
     #:with [name-get-state ...] (map (lambda (name-stx) (format-id name-stx "~a-get-state" name-stx #:source name-stx #:props name-stx))
                                  (attribute name))
     #'(begin
         (lambda ()
           (define (periodic-loop counter)
             (if (= 0 (modulo counter period))
               (write (serialize (name-get-state)) port)
                ; TODO ensure that it gets buffered, and in the other end, we should read while there are lines available, before updating the bar.
                (void)) ...
             (sleep 1)
             (periodic-loop (+ 1 counter)))
           (periodic-loop 0)))]
    [(_) #'(void)]))

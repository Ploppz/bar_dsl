#lang racket
; A very minimal expander (also called "module language" https://docs.racket-lang.org/guide/module-languages.html)
;   - only provides widgets and necessities.

; Widgets
(define-syntax-rule (provide-from module-path)
  (begin (require module-path)
    (provide (all-from-out module-path))))
(provide-from "widget/mpd.rkt")

; Necessities
;(provide #%module-begin #%app #%datum #%top require define-values make-pipe define thread list lambda match)
(provide (all-from-out racket))

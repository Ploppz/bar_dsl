#lang racket
; A very minimal expander (also called "module language" https://docs.racket-lang.org/guide/module-languages.html)

; Widgets
(define-syntax-rule (provide-from module-path)
  (begin (require module-path)
    (provide (all-from-out module-path))))
(provide-from "widget/mpd.rkt")

; Necessities
(provide (all-from-out racket))

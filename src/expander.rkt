#lang racket
; A very minimal expander: only provides widgets.


(define-syntax-rule (module-begin PROGRAM ...)
  #'(#%module-begin PROGRAM ...))

(provide (rename-out [module-begin #%module-begin]))

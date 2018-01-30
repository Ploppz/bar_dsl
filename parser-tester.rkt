#lang racket
(require "src/parser.rkt"
         "src/reader.rkt"
         megaparsack
         data/monad ; do
         data/applicative ; pure
         racket/pretty)

(define (reduce gen)
  (define token (gen))
  (if (false? token)
    (list)
    (cons token (reduce gen))))

(define next-token (make-tokenizer (open-input-file "test-bar")))

(define tokens (reduce next-token))
(pretty-print tokens)

;(define p/p (do (token/p 'WORD)))
;(display (parse-result! (parse-tokens p/p tokens)))

(define result (parse-result! (parse bar/p tokens)))
(pretty-print result)

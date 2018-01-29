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

(define next-token (make-tokenizer
  (open-input-string
    ; "(define a 'something) run ram-loop [ram max-ram => (format \"~a/~a GB RAM\" ram max-ram)] {ram} {fg:#966} {cpu} Some text (leftsep #363)"))))
    "(define a 'something) run ram-loop [ram max-ram => (format \"~a/~a GB RAM\" ram max-ram)]."
    ; "="
    )))

(define tokens (reduce next-token))
(pretty-print tokens)

;(define p/p (do (token/p 'WORD)))
;(display (parse-result! (parse-tokens p/p tokens)))

(define result (parse-result! (parse bar/p tokens)))
(display result)
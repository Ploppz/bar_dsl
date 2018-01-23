#lang racket
(require "parser.rkt")
(require "reader.rkt")
(define (displaylist l)
  (cond
    [(empty? l) 0]
    [else (displayln (first l)) (displaylist (rest l))]))
(define tokens
  (reduce
    (make-tokenizer
      (open-input-string
        "(define a 'something) run ram-loop [ram max-ram => (format \"~a/~a GB RAM\" ram max-ram)] {ram} {fg:#966} {cpu} Some text (leftsep #363)"))))
(displaylist tokens)
(parse tokens)

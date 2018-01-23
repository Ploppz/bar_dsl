#lang racket
(require ragg/support)

(define (make-tokenizer port)
  (port-count-lines! port)
  (define (next-token)
    (define c (peek-char port))
    (define loc (call-with-values (lambda () (port-next-location port)) list))
    (match-define (list line col _) loc)
    (cond
          [(eof-object? c) #f]
          [(char=? c #\() (token 'SEXPR (balance-brackets port #\( #\)) #:line line #:column col)] ; S-expression
          [(char-word? c) (token 'WORD (parse-word port) #:line line #:column col)] ; word
          [(char-whitespace? c) (read-char port) (next-token)]
          [else (token c (read-string 1 port) #:line line #:column col)]))
  next-token)
(provide make-tokenizer)

(define (char-word? c)
  (or (char-alphabetic? c)
      (char-numeric? c)
      (char=? c #\-)
      (char=? c #\-)))

; Returns the parsed word
(define (parse-word port)
  (define (parse result)
    (define c (peek-char port))
    (if (char-word? c)
      (parse (string-append result (read-string 1 port)))
      result))
  (parse ""))


(define (balance-brackets port left right)
  ; TODO: get indices instead of string-append.
  (define (balance level result)
    (define c (read-char port))
    (if (eof-object? c)
        result
        (cond
              [(char=? c left) (balance (+ 1 level) (string-append result (string c)))]
              [(char=? c right) (if (= level 1)
                      (string-append result (string c))
                      (balance (- 1 level) (string-append result (string c))))]
              [else (balance level (string-append result (string c)))])))
  (balance 0 ""))

;;;;; Testing
(define (reduce gen)
  (define token (gen))
  (if (false? token)
    (list)
    (cons token (reduce gen))))
(provide reduce)
;(define a (make-tokenizer (open-input-file "../bar2.rkt")))
;(reduce a)

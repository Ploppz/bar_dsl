#lang racket
(require megaparsack megaparsack/text)
(require parser-tools/lex) ; for position-token & parse-tokens
(require megaparsack/parser-tools/lex) ; for token/p
(require data/monad) ; for do
(require data/applicative) ; for pure

; (position offset line col)
(define tokens (list (position-token 'A (position 1 1 0) (position 2 1 1))
                     (position-token 'S (position 2 1 1) (position 6 1 5))))
tokens

(define p/p (do (token/p 'A) (token/p 'S) (pure "SUCCESS")))
(define stx (parse-result! (parse-tokens p/p tokens)))
stx

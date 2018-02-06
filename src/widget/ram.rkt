#lang racket/base

(provide ram-get-state ram-state)

(require racket/serialize
         racket/date
         data/monad ; for do
         data/applicative ; for pure
         megaparsack
         megaparsack/text)

(define (col/p name) (do (string/p name) (many/p space/p) [num <- integer/p] space/p (pure num)))

(struct ram-state (used total) #:prefab)

(define (ram-get-state)
  (define f (open-input-file "/proc/meminfo"))
  (define tot (parse-result! (parse-string (col/p "MemTotal:") (read-line f))))
  (void (read-line f))
  (define avail (parse-result! (parse-string (col/p "MemAvailable:") (read-line f))))
  (ram-state (- tot avail) tot))


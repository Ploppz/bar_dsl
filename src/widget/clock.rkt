#lang racket
(provide clock-get-state clock-state)

(require racket/serialize racket/date)

(struct clock-state (h m s) #:prefab)

(define (clock-get-state)
  (define d (current-date))
  (clock-state (date-hour d) (date-minute d) (date-second d)))

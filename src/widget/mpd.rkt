#lang racket
(provide mpd-loop mpd-state)

(require racket/serialize)

; https://rosettacode.org/wiki/Get_system_command_output#Racket
(define (subcmd cmd) (regexp-split #px"\n" (with-output-to-string (lambda () (system cmd)))))

(struct mpd-state (state) #:prefab)

(define (mpd-get-state)
  (define lines (subcmd "mpc"))
  (define state
    (if (> (length lines) 1)
      (match (regexp-match #px"\\[.*\\]" (list-ref lines 1))
             ['("[playing]")    'playing]
             ['("[paused]")     'paused]
             [#f                'stopped])
      'stopped))
  (mpd-state state))

; mpc -f '[[%{F##ff801D3A}%title%][%{F##ff7c5350} by %{F##ff5F176C}%artist%]]|%file%'

(define (mpd-loop port)
  (define (loop)
    (system "mpc idle player >/dev/null")
    (write (serialize (mpd-get-state)) port)
    (loop))
  (loop))
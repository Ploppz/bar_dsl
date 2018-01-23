#lang racket
; https://rosettacode.org/wiki/Get_system_command_output#Racket

(define (subcmd-lines cmd) (regexp-split #px"\n" (with-output-to-string (lambda () (system cmd)))))

(define (mpd-state)
  (define lines (subcmd-lines "mpc"))
  (define state
    (if (> (length lines) 1)
      (match (regexp-match #px"\\[.*\\]" (list-ref lines 1))
             ['("[playing]")    'playing]
             ['("[paused]")     'paused]
             [#f                'stopped])
      'stopped))
  (list state)
  )

; mpc -f '[[%{F##ff801D3A}%title%][%{F##ff7c5350} by %{F##ff5F176C}%artist%]]|%file%'

(define (mpd-listener)
  (define (loop)
    (system "mpc idle player >/dev/null")
    (yield (mpd-state))
    (loop))
  (loop))

; PROTOTYPING

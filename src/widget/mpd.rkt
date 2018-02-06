#lang racket/base
(provide mpd-loop mpd-state)

(require racket/serialize
         racket/match
         racket/port
         racket/system)

; https://rosettacode.org/wiki/Get_system_command_output#Racket
(define (subcmd cmd) (regexp-split #px"\n" (with-output-to-string (lambda () (system cmd)))))

(struct mpd-state (state title artist album) #:prefab)

(define (mpd-get-state)
  (define lines (subcmd "mpc -f \"%title%\n%artist%\n%album%\""))
  (match lines
    [(list title artist album info _ _)
      (define state
          (match (regexp-match #px"\\[.*\\]" info)
                 ['("[playing]")    'playing]
                 ['("[paused]")     'paused]
                 [#f                'stopped]))
      (mpd-state state title artist album)]
    [else (mpd-state 'stopped "" "" "")]))

; mpc -f '[[%{F##ff801D3A}%title%][%{F##ff7c5350} by %{F##ff5F176C}%artist%]]|%file%'

(define (mpd-loop port)
  (define (loop)
    (write (serialize (mpd-get-state)) port)
    (system "mpc idle player >/dev/null")
    (loop))
  (loop))

; The C lib:
; mpd_status_get_state

#lang reader "src/reader.rkt"

start mpd [state title artist album =>
            (cond
              [(eq? state 'playing) (format "~a - ~a" title artist)]
              [else "(stopped)"])]

(define (pad2 num) (~r num #:min-width 2 #:pad-string "0"))
period 1 clock [h m s => (format "~a:~a:~a" (pad2 h) (pad2 m) (pad2 s))]

'(define (green) "%{F#67BA28}")
@right Music: %{F#d65} ' '[mpd] '(green) '[clock]

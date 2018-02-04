#lang reader "src/reader.rkt"
(define (foo) "Foo")
start mpd [state title artist album => (cond
                                         [(eq? state 'playing) (format "~a - ~a" title artist)]
                                         [else "(stopped)"])]
period 1 clock [h m s => (format "~a:~a:~a" h m s)]
@right Music: %{F#d65} ' '[mpd] '(foo) '[clock]

#lang reader "src/reader.rkt"
(define (foo) "Foo")
start mpd [state => (format "~a" state)]
period 1 clock [h m s => (format "~a:~a:~a" h m s)]
@right Music: %{F#d65} '[mpd] '(foo) '[clock]'

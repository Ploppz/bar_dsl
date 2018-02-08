#lang reader "src/reader.rkt"
; Useful from the Racket reference:
; Converting Values to String:
;  https://docs.racket-lang.org/reference/strings.html#%28part._format%29

; Music information from mpd
start mpd [state title artist album =>
            (cond
              [(eq? state 'playing) (format "♪ ~a - ~a ♪" title artist)]
              [else ""])]

; Desktop information from BSPWM
(define (format-desktop desktop)
  (match desktop
   [(bspwm-desktop name _ #t _) (format "%{F#FFF}~a" name)]
   [(bspwm-desktop name _ #f _) (format "%{F#644}~a" name)]))
start bspwm [desktops _ _ =>
              (string-join (map format-desktop desktops) " ")]

; RAM
(define (to-gb kb precision)
  (~r (/ kb 1000000.0) #:precision precision))
period 1 ram [used total => (format " ~a/~a GB RAM" (to-gb used 3) (to-gb total 0))]

; Clock
(define (pad2 num) (~r num #:min-width 2 #:pad-string "0"))
period 1 clock [h m s => (format "~a:~a:~a" (pad2 h) (pad2 m) (pad2 s))]

(define (transparent) "%{B#00000000}")
(define (green) "%{F#67BA28}")
@left '[ram]
@center %{B#000} '[bspwm] '(transparent)
@right %{F#d65} '[mpd] '(green) '[clock]

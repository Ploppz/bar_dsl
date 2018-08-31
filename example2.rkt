#lang reader "src/reader.rkt"
; TODO: The fg & bg should be given, but dependent on the target bar, e.g. lemonbar, polybar, ...
; Colors!
(define (fg col) (format "%{F~a}" col))
(define (bg col) (format "%{B~a}" col))
(define transparent "#00000000")
(define bg0 "#111111")
(define bg1 "#1F4B17")
(define bg2 "#111111")
(define bg3 "#497933")

; Stuff for left (L) and right (R) separators
(define cur-bg "#000")
(define (R new-bg)
   (let [(old-bg cur-bg)]
     (set! cur-bg new-bg)
     (format "%{F~a} %{B~a}⮀⮁%{F#FFF} " old-bg new-bg)))
(define (L new-bg)
   (let [(old-bg cur-bg)]
     (set! cur-bg new-bg)
     (format "%{F~a} %{B~a} ⮃⮂%{F#FFF}%{B#111}" new-bg old-bg)))


;; Music information from mpd
start mpd [state title artist album =>
            (cond
              [(eq? state 'playing) (format "♪ ~a - ~a ♪" title artist)]
              [else ""])]

;; Desktop information from BSPWM
(define greek-big (list #\Α #\Β #\Γ #\Δ #\Ε #\Ζ #\Η #\Θ #\Ι #\Κ #\Λ #\Μ #\Ν #\Ξ #\Ο #\Π #\Ρ #\Σ #\Τ #\Υ #\Φ #\Χ #\Ψ #\Ω))
(define greek-small (list #\α #\β #\γ #\δ #\ε #\ζ #\η #\θ #\ι #\κ #\λ #\μ #\ν #\ξ #\ο #\π #\ρ #\σ #\τ #\υ #\φ #\χ #\ψ #\ω))
(define (format-desktop desktop separator)
  (define sep (if separator "%{F#FFF}|" ""))
  (define (make-big-name name) (list-ref greek-big (string->number name)))
  (define (make-small-name name) (list-ref greek-small (string->number name)))
  (match desktop
   [(bspwm-desktop name _ #t _) (format "%{B#9a1} %{F#000}~a %{B#111} ~a" (make-big-name name) sep)]
   [(bspwm-desktop name #t #f _) (format "%{B#111} %{F#9a1}~a ~a" (make-big-name name) sep)]
   [(bspwm-desktop name #f #f _) (format "%{B#111} %{F#554}~a ~a" (make-small-name name) sep)]))

start bspwm [desktops monocle wmode =>
              (format "%{F#FAC}~a ~a ~a ~a %{F#FAC}~a"
                (symbol->string wmode)
                (L bg0)
                (string-join (for/list [(desktop desktops)
                                        (i '(#f #f #t #f #f #t #f #f #f))]
                                       (format-desktop desktop i)) "")
                (R transparent)
                (if monocle "monocle" "tile"))]

;; RAM
(define (to-gb kb precision)
  (~r (/ kb 1000000.0) #:precision precision))
period 1 ram [used total => (format " ~a/~a GB RAM" (to-gb used 3) (to-gb total 0))]

;; Clock
(define (pad2 num) (~r num #:min-width 2 #:pad-string "0"))
period 1 clock [h m s => (format "~a:~a:~a" (pad2 h) (pad2 m) (pad2 s))]

@left '(fg "#FFF") '(bg bg0) '[ram] '(R bg1) ⭦ 1.2% '(R bg2) ⮞ 34 '(R bg3) '[mpd] '(R transparent)
@center '[bspwm]
@right %{B#497933} '[clock]

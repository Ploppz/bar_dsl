#lang reader "src/reader.rkt"
; TODO: The fg & bg should be given, but dependent on the target bar, e.g. lemonbar, polybar, ...
; Colors!
(define (fg col) (format "%{F~a}" col))
(define (bg col) (format "%{B~a}" col))
(define transparent "#00000000")
(define bg0 "#3F6C2F")
(define bg1 "#6C612F")
(define bg2 "#6C2F3D")
(define bg3 "#432F6C")

; Stuff for left (L) and right (R) separators
(define cur-bg "xxx")
(define (open bg)
  (set! cur-bg bg)
  (format "  %{F~a}⮂%{B~a}%{F#FFF}" bg bg))
(define (close)
  (format "%{B#00000000}%{F~a}⮀  " cur-bg))


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
   [(bspwm-desktop name _ #t _) (format  "%{B#fea}%{F#000} ~a %{B#000}~a" (make-big-name name) sep)]
   [(bspwm-desktop name #t #f _) (format "%{B#000}%{F#FFF} ~a ~a" (make-big-name name) sep)]
   [(bspwm-desktop name #f #f _) (format "%{B#000} %{F#554}~a ~a" (make-small-name name) sep)]))

start bspwm [desktops monocle wmode =>
              (format "~a ~a ~a ~a ~a ~a ~a ~a ~a"
                (open bg0)
                (symbol->string wmode)
                (close)
                (open "#000")
                (string-join (for/list [(desktop desktops)
                                        (i '(#f #f #t #f #f #t #f #f #f))]
                                       (format-desktop desktop i)) "")
                (close)
                (open bg1)
                (if monocle "monocle" "tile")
                (close))]

;; RAM
(define (to-gb kb precision)
  (~r (/ kb 1000000.0) #:precision precision))
period 1 ram [used total => (format " ~a/~a GB RAM" (to-gb used 3) (to-gb total 0))]

;; Clock
(define (pad2 num) (~r num #:min-width 2 #:pad-string "0"))
period 1 clock [h m s => (format "~a:~a:~a" (pad2 h) (pad2 m) (pad2 s))]

@left %{U#FFF} '(fg "#FFF") '(open bg0) '[ram] '(close) '(open bg1) ⭦ 1.2% '(close) '(open bg2) ⮞ 34 '(close) '(open bg3) '[mpd] '(close) '(bg transparent)
@center '[bspwm] '(bg transparent)
@right '(open bg3) '[clock] '(close) '(bg transparent)

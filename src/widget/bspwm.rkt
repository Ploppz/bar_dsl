#lang racket/base
(provide bspwm-loop
         bspwm-state
         (struct-out bspwm-desktop)
         (struct-out bspwm-state))

(require racket/serialize
         racket/match
         racket/string
         racket/list
         racket/system)

(struct bspwm-desktop (name occupied focused urgent) #:prefab)
(struct bspwm-state (desktops monocle window-mode) #:prefab)

(define (parse-wmode c) ; Parse Window Mode
  (match c
      [#\T 'tiled]
      [#\P 'pseudo]
      [#\F 'floating]
      [#\= 'full]))
(define (parse-bspwm-desktop str)
  (define flag (string-ref str 0))
  (define occupied (or (char=? flag #\o) (char=? flag #\O)))
  (define focused (or (char=? flag #\F) (char=? flag #\O) (char=? flag #\U)))
  (define urgent (or (char=? flag #\u) (char=? flag #\U)))
  (bspwm-desktop (substring str 1) occupied focused urgent))

(define (parse-state str)
  (define strs (string-split (symbol->string str) ":"))
  (define (loop elements desktops monocle wmode)
    (cond
      [(empty? elements) (values (reverse desktops) monocle wmode)]
      [else
        (define elem (first elements))
        (match (string-ref elem 0)
               [(or #\o #\O #\f #\F #\u #\U) (loop (rest elements) (cons (parse-bspwm-desktop elem) desktops) monocle wmode)]
               [#\L (loop (rest elements) desktops (char=? (string-ref elem 1) #\M) wmode)]
               [#\T (loop (rest elements) desktops monocle (parse-wmode (string-ref elem 1)))]
               [_ (loop (rest elements) desktops monocle wmode)])]))
  (define-values (desktops monocle wmode) (loop strs (list) #f 'none))
  (bspwm-state desktops monocle wmode))

(define (bspwm-loop port)
  (match-define (list stdout stdin pid stderr fn) (process "bspc subscribe"))
  ; ^ We only care about stdout
  (define (loop)
    (write (serialize (parse-state (read stdout))) port)
    (flush-output)
    (loop))
  (loop))

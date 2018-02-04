#lang racket
(provide bar/p)
(require "lex.rkt"
         megaparsack
         data/monad ; for do
         data/applicative) ; for pure

;;; Helpers
(define (text-char? tok)
  (match tok
         [(token 'CHAR val) (nor (string=? val "\n") (string=? val "'"))]
         [_ #f]))
(struct widget (name)) ; For the layout
(struct sexpr (datum)) ; For the layout

;;;;;;;;;;;
;;; PARSING

(define sexpr/p
  (do [val <- (token-type/p 'SEXPR)] (pure (sexpr/code val))))
(define space/p (or/p (token/p "\n") (token/p " ") (token/p "\t")))
(define spaces/p (many/p (hidden/p space/p)))

;; program : SEXPR* start* layout+
(define bar/p (do
                spaces/p
                [inits <- (many/p (do [sexpr <- sexpr/p] spaces/p (pure sexpr)))]
                [starts <- (many/p start/p)]
                [layout <- layout/p]
                eof/p
                (pure (bar/code inits starts layout))))
;; start   : ("start" | "period" INT) WORD "[" WORD* "=" ">" sexpr "]"
(define int/p (do [digits <- (many/p (satisfy/p (lambda (tok) (char-numeric? (string-ref (token-value tok) 0)))))]
                  (pure (string->number
                          (bytes->string/utf-8
                            (list->bytes
                              (map (lambda (tok) (char->integer (string-ref (token-value tok) 0)))
                                   digits)))))))
(define start/p (do
                  [periodic <- (or/p
                    (do (token/p "start") (pure #f))
                    (do (token/p "period") spaces/p [period <- int/p] (pure period)))]
                  spaces/p
                  [start-name <- (token-type/p 'WORD)] spaces/p
                  (token/p "[") spaces/p
                  [params <- (many/p (do [param <- (token-type/p 'WORD)] spaces/p (pure param)))] 
                  (token/p "=") (token/p ">") spaces/p
                  [transform <- sexpr/p] spaces/p
                  (token/p "]") spaces/p
                  (pure (start/code start-name params transform periodic))))



;; text    : WORD | (any char but \n) | "\'"
; Returns a string (possibly part of a bigger)
(define text/p (do [text-token <- (or/p (token-type/p 'WORD)
                                        (do (token/p "\\") (token/p "'") (pure (token 'CHAR "'")))
                                        (satisfy/p text-char?))]
                   (pure (token-value text-token))))
;; widget  : "[" WORD "]"
(define widget/p (do (token/p "[")
                     [name <- (token-type/p 'WORD)]
                     (token/p "]")
                     (pure (widget (token-value name)))))
;; special : "'" (sexpr | widget)
(define special/p (do (token/p "'")
                    (or/p
                      (do [datum <- sexpr/p] (pure (sexpr datum)))
                      widget/p
                      (do (pure "'"))))) ; <-- If all else fails, treat "'" as text!
;; elem    : text | special
(define element/p (or/p text/p special/p))
;; orientation : ("@left" | "@right" | "@center")
(define orientation/p (do (token/p "@") [ori <- (or/p (do (token/p "left") (pure 'left))
                                              (do (token/p "right") (pure 'right))
                                              (do (token/p "center") (pure 'center)))]
                        (pure ori)))
;; layout  : (orientation elem*)+
; Returns a nested list which can safely be flattened
(define layout/p (many/p (do [ori <- orientation/p]
                             spaces/p
                             [elems <- (many/p element/p)]
                             spaces/p
                             (pure (list ori elems))) #:min 1 #:max 3))



;;;;;;;;;;;;;;;;;;;;
;;; CODE TRANSLATION

(define (bar/code inits starts nested-layout)
  (define start-inits (map car starts))
  (define (pred x) (match (cadr x) [(list 'PERIODIC code) code] [_ #f]))
  (define start-periodics (filter-map pred starts))
  (define start-threads (map cadr (filter (negate pred) starts)))
  (define start-patterns (map caddr starts))
  ; Make a namespace and execute the initial statements with it
  (define ns (make-base-namespace))
  (map (lambda (datum) (eval datum ns)) inits)
  ; Structure the layout
  (define layout (layout-structure nested-layout ns))
  ; Code
  `(module bar-mod "src/expander.rkt"
    (require racket/serialize)
    (define-values (pipe-in pipe-out) (make-pipe))
    ,@start-inits
    ,@start-threads
    (define periodic-thread (thread (start-periodic-loop pipe-out ,@start-periodics))) ; Given by expander
    (define layout (list ,@layout))
    (define (loop)
      (define raw-obj (read pipe-in))
      (define obj (deserialize raw-obj))
      (match obj ,@start-patterns)
      (for ([element layout])
           (cond
             [(procedure? element) (display (element))]
             [else (display element)]))
      (display "\n")
      (flush-output)
      (loop))
    (loop)))

(define (start/code start-name params transform period)
  (define name (token-value start-name))
  (define loop-name (string->symbol (format "~a-loop" name)))
  (define thread-name (string->symbol (format "~a-thread" name)))
  (define state-var (string->symbol (format "~a-value" name)))
  (define state-type (string->symbol (format "~a-state" name)))
  (list 
    `(define ,state-var "") ; Code to init state
    (if period
      (list 'PERIODIC `(,(string->symbol name) ,period))
      `(define ,thread-name (thread (lambda () (,loop-name pipe-out))))) ; Code to start thread
    `[(,state-type ,@(map token->ident params)) ; Match clause
      (set! ,state-var ,transform)]))

(define (sexpr/code tok)
  (match tok
    [(token name val)
     (read (open-input-string val))]))

(define (token->ident tok)
  (match tok [(token 'WORD val)
              (string->symbol val)]))


(define (layout-structure nested-layout namespace)
  (concat-subsequent-strings
    (layout-transform (flatten nested-layout) namespace)))

(define (layout-transform flat-layout namespace)
  ; lemonbar format. In the future one can use other kinds of formats
  (define (transform elem)
    (define (widget->code name)
      (define sym (string->symbol (format "~a-value" name)))
      `(lambda () ,sym))
    (match elem
           ['left "%{l}"]
           ['right "%{r}"]
           ['center "%{c}"]
           [(widget name) (widget->code name)]
           [(sexpr datum) (define val (eval datum namespace))
                          (if (string? val) val
                            (error "Function call in layout must return string!"))] ;TODO Proper error...
           [else elem]))
  (map transform flat-layout))
(define (concat-subsequent-strings l) ; Concatenate all subsequent strings in a list
  (define (g l str-acc)
    (cond
      [(empty? l) l]
      [else (define head (first l))
            (define tail (rest l))
            (cond
              [(string? head) (g tail (string-append str-acc head))]
              [else (cond
                      [(non-empty-string? str-acc) (cons str-acc (cons head (g tail "")))]
                      [else                        (cons head (g tail ""))])])]))
  (g l ""))

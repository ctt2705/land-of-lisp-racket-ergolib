#lang racket

(define *nodes* '(
  (living-room (you are in the living-room.
                a wizard is snoring loudly on the couch.))
  (garden (you are in a beautiful garden.
           there is a well in front of you.))
  (attic (you are in the attic.
          there is a giant welding torch in the corner.))
))

(define (describe-location location nodes)
  (cadr (assoc location nodes))
)

(define *edges* '(
  (living-room (garden west door)
               (attic upstairs ladder))
  (garden (living-room east door))
  (attic (living-room downstairs ladder))
))

(define (describe-path edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.)
)

(define (describe-paths location edges)
  (apply append (map describe-path
                     (cdr (assoc location edges))
  )))

(define *objects* '(whiskey bucket frog chain))
(define *object-locations* '(
  (whiskey living-room)
  (bucket living-room)
  (chain garden)
  (frog garden)
))

(define (objects-at loc objs obj-locs)
  (let ([at-loc? (lambda (obj)
                   (eq? (cadr (assoc obj obj-locs))
                        loc))])
    (filter at-loc? objs)
  ))

(define (describe-objects loc objs obj-loc)
  (let ([describe-obj (lambda (obj)
                        `(you see a ,obj on the floor.))])
    (apply append (map describe-obj
                       (objects-at loc objs obj-loc)))
  ))

(define *location* 'living-room)
(define (look)
  (append
   (describe-location *location* *nodes*)
   (describe-paths *location* *edges*)
   (describe-objects *location* *objects* *object-locations*)
  ))

;; Emulates CL find
(define (find what lst #:key [proc (lambda (x) x)])
  (findf (lambda (current) (eq? what (proc current)))
         lst
  ))

(define (walk direction)
  (let ([next (find direction
                    (cdr (assoc *location* *edges*))
                    #:key cadr)])
    (if next
        (begin (set! *location* (car next))
               (look))
        '(you cannot go that way.)
    )))

;; Emulates CL push (only works for list)
(define-syntax push!
  (syntax-rules ()
    [(push! elem lst) (set! lst (cons elem lst))]
  ))

(define (pickup object)
  (cond
    ((member object
             (objects-at *location* *objects* *object-locations*))
      (push! (list object 'body) *object-locations*)
      `(you are now carrying the ,object)
    )
    (else '(you cannot get that.))
  ))

(define (inventory)
  (cons 'items- (objects-at 'body *objects* *object-locations*))
)

(define (game-repl)
  (let ([cmd (game-read)])
    (unless (eq? (car cmd) 'quit)
      (game-print (game-eval cmd))
      (game-repl)
    )))

;; Emulates CL read-from-string
(define (read-from-string str)
  (let ([sp (open-input-string str)])
    (read sp)
  ))

(define (game-read)
  (let ([cmd (read-from-string (string-append "(" (read-line) ")"))])
    (let ([quote-it (lambda (x)
                      (list 'quote x))])
      (cons (car cmd) (map quote-it (cdr cmd)))
    )))

(define *allowed-commands* '(look walk pickup inventory))

(define (game-eval sexp)
  (if (member (car sexp) *allowed-commands*)
    (eval sexp)
    '(i do not know that command.)
  ))

(define (tweak-text lst caps lit)
  (if (empty? lst)
    null ; necessary for list->string in calling function
    (let ([item (car lst)]
          [rest (cdr lst)])
      (cond
        [(eq? item #\space)
          (cons item (tweak-text rest caps lit))]
        [(member item '(#\! #\? #\.))
          (cons item (tweak-text rest #t lit))]
        [(eq? item #\")
          (tweak-text rest caps (not lit))]
        [lit
          (cons item (tweak-text rest #f lit))]
        [caps
          (cons (char-upcase item) (tweak-text rest #f lit))]
        [else
          (cons (char-downcase item) (tweak-text rest #f #f))]
      ))))

;; string-trim works differently from CL's
(require racket/string)
(define (trim-parens str)
  (string-trim (string-trim str ")") "(")
)

(define (game-print lst)
  (display (list->string (tweak-text (string->list (trim-parens (~a lst)))
                                     #t
                                     #f)))
  (newline)
)

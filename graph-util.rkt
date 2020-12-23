#lang racket

(define *wizard-nodes* '(
  (living-room (you are in the living-room.
                   a wizard is snoring loudly on the couch.))
  (garden (you are in a beautiful garden.
            there is a well in front of you.))
  (attic (you are in the attic.
          there is a giant welding torch in the corner.))
))
(define *wizard-edges* '(
  (living-room (garden west door)
               (attic upstairs ladder))
  (garden (living-room east door))
  (attic (living-room downstairs ladder))
))

;; Emulates CL substitute-if (only for string)
(define (substitute-if new-val proc str)
  (list->string (map (lambda (current)
         (if (proc current)
           new-val
           current
         ))
       (string->list str)
  )))

;; Emulates CL complement
(define (complement proc)
  (lambda (args)
    (if (proc args)
      #f
      #t
    )))

;; Emulates CL alphanumericp
(define (alphanumeric? ch)
  (or (char-alphabetic? ch) (char-numeric? ch))
)

(define (dot-name exp)
  (substitute-if #\_ (complement alphanumeric?) (~a exp))
)

(define *max-label-length* 30)

(define (dot-label exp)
  (if (empty? exp)
    ""
    (let  ([s (~a exp)])
      (if (> (string-length s) *max-label-length*)
        (string-append (substring s 0 (- *max-label-length* 3)) "...")
        s
      ))))

(define (nodes->dot nodes)
  (for-each (lambda (node)
              (newline)
              (display (dot-name (car node)))
              (display "[label=\"")
              (display (dot-label node))
              (display "\"];"))
            nodes
  ))

(define (edges->dot edges)
  (for-each (lambda (node)
              (for-each (lambda (edge)
                          (newline)
                          (display (dot-name (car node)))
                          (display "->")
                          (display (dot-name (car edge)))
                          (display "[label=\"")
                          (display (dot-label (cdr edge)))
                          (display "\"];"))
                        (cdr node)
              ))
            edges
  ))

(define (graph->dot nodes edges)
  (display "digraph{")
  (nodes->dot nodes)
  (edges->dot edges)
  (display "}")
)

(define (dot->png fname thunk)
  (with-output-to-file fname #:exists 'replace
    (lambda () (thunk))
  )
  (system (string-append "dot -Tpng -O " fname))
)

(define (graph->png fname nodes edges)
  (dot->png fname
            (lambda ()
              (graph->dot nodes edges)))
)

;; Emulate CL maplist—only works for a single list
(define (mapcdr proc remainder #:acc [acc '()])
  (if (empty? remainder)
    acc
    (mapcdr proc (cdr remainder) #:acc (append acc (list (proc remainder))))
  ))

(define (uedges->dot edges)
  (mapcdr (lambda (lst)
                   (for-each (lambda (edge)
                               (unless (assoc (car edge) (cdr lst))
                                 (newline)
                                 (display (dot-name (caar lst)))
                                 (display "--")
                                 (display (dot-name (car edge)))
                                 (display "[label=\"")
                                 (display (dot-label (cdr edge)))
                                 (display "\"];")
                               ))
                             (cdar lst)
                   ))
                 edges
  ))

(define (ugraph->dot nodes edges)
  (display "graph{")
  (nodes->dot nodes)
  (uedges->dot edges)
  (display "}")
)

(define (ugraph->png fname nodes edges)
  (dot->png fname
            (lambda ()
              (ugraph->dot nodes edges)))
)

(provide ugraph->png) ; required for wumpus.rkt (next chapter)


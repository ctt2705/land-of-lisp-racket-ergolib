(load "ergolib/init.lisp")
(require :ergolib)
(define-synonym alphanumeric? alphanumericp)
(define-synonym display princ)

(defc wizard-nodes '(
  (living-room (you are in the living-room.
                a wizard is snoring loudly on the couch.))
  (garden (you are in a beautiful garden.
           there is a well in front of you.))
  (attic (you are in the attic.
          there is a giant welding torch in the corner.))
))

(defc wizard-edges '(
  (living-room (garden west door) (attic upstairs ladder))
  (garden (living-room east door))
  (attic (living-room downstairs ladder))
))

(defun dot-name (exp)
  (substitute-if #\_ (complement #'alphanumeric?) (prin1-to-string exp))
)

(defc max-label-length 30)

(defun dot-label (exp)
  (if exp
    (bb s (write-to-string exp :pretty nil)
      (if (> (length s) max-label-length)
        (cat (subseq s 0 (- max-label-length 3)) "...")
        s
      ))
    ""
  ))

(defun nodes->dot (nodes)
  (for node in nodes do
    (fresh-line)
    (display (dot-name (car node)))
    (display "[label=\"")
    (display (dot-label node))
    (display "\"];")
  ))

(defun edges->dot (edges)
  (for node in edges do
    (for edge in (cdr node) do
      (fresh-line)
      (display (dot-name (car node)))
      (display "->")
      (display (dot-name (car edge)))
      (display "[label=\"")
      (display (dot-label (cdr edge)))
      (display "\"];")
    )))

(defun graph->dot (nodes edges)
  (display "digraph{")
  (nodes->dot nodes)
  (edges->dot edges)
  (display "}")
)

(defun dot->png (fname thunk)
  (with-open-file (*standard-output* fname
                   :direction :output
                   :if-exists :supersede)
    (funcall thunk)
  )
  (ext:shell (cat "dot -Tpng -O " fname))
)

(defun graph->png (fname nodes edges)
  (dot->png fname
           (fn () (graph->dot nodes edges)))
)

(defun uedges->dot (edges)
  (mapcdr (fn (lst)
            (for edge in (cdar lst) do
              (unless (assoc (car edge) (cdr lst))
                (fresh-line)
                (display (dot-name (caar lst)))
                (display "--")
                (display (dot-name (car edge)))
                (display "[label=\"")
                (display (dot-label (cdr edge)))
                (display "\"];")
              )))
          edges
  ))

(defun ugraph->dot (nodes edges)
  (display "graph{")
  (nodes->dot nodes)
  (uedges->dot edges)
  (display "}")
)

(defun ugraph->png (fname nodes edges)
  (dot->png fname
            (fn ()
              (ugraph->dot nodes edges)))
)


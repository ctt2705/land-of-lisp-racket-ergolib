(load "ergolib/init.lisp")
(require :ergolib)
(define-synonym eq? eq)
(define-synonym set! setf)
(define-synonym push! push)
(define-synonym display princ)

(defc nodes '(
  (living-room (you are in the living-room.
                a wizard is snoring loudly on the couch.))
  (garden (you are in a beautiful garden.
           there is a well in front of you.))
  (attic (you are in the attic.
          there is a giant welding torch in the corner.))
))

(defun describe-location (location nodes)
  (cadr (assoc location nodes))
)

(defc edges '(
  (living-room (garden west door) (attic upstairs ladder))
  (garden (living-room east door))
  (attic (living-room downstairs ladder))
))

(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.)
)

(defun describe-paths (location edges)
  (mappend #'describe-path (cdr (assoc location edges)))
)

(defc objects '(whiskey bucket frog chain))
(defv object-locations '(
  (whiskey living-room)
  (bucket living-room)
  (chain garden)
  (frog garden)
))

(defun objects-at (loc objs obj-locs)
  (bb :fn at-loc? (obj)
            (eq? (cadr (assoc obj obj-locs)) loc)
    (filter objs #'at-loc?)
  ))

(defun describe-objects (loc objs obj-loc)
  (bb :fn describe-obj (obj)
            `(you see a ,obj on the floor.)
    (mappend #'describe-obj (objects-at loc objs obj-loc))
  ))

(defv location 'living-room)

(defun look ()
  (append (describe-location location nodes)
          (describe-paths location edges)
          (describe-objects location objects object-locations))
)

(defun walk (direction)
  (bb next (find direction
                 (cdr (assoc location edges))
                 :key #'cadr)
    (if next
      (progn (set! location (car next))
             (look))
      '(you cannot go that way.)
    )))

(defun pickup (object)
  (mcond
    (member object (objects-at location objects object-locations))
      (progn (push! (list object 'body) object-locations)
             `(you are now carrying the ,object))
    '(you cannot get that.)
  ))

(defun inventory ()
  (cons 'items- (objects-at 'body objects object-locations))
)

(defun game-repl ()
  (bb cmd (game-read)
    (unless (eq? (car cmd) 'quit)
      (game-print (game-eval cmd))
      (game-repl)
    )))

(defun game-read ()
  (bb cmd (read-from-string (cat "(" (read-line) ")"))
    (bb :fn quote-it (x) (list 'quote x)
      (cons (car cmd) (mapcar #'quote-it (cdr cmd)))
    )))

(defc allowed-commands '(look walk pickup inventory))

(defun game-eval (sexp)
  (if (member (car sexp) allowed-commands)
    (eval sexp)
    '(i do not know that command.)
  ))

(defun tweak-text (lst caps lit)
  (when lst
    (bb item (car lst)
        rest (cdr lst)
      (mcond
        (eq? item #\space)
          (cons item (tweak-text rest caps lit))
        (member item '(#\! #\? #\.))
          (cons item (tweak-text rest t lit))
        (eq? item #\")
          (tweak-text rest caps (not lit))
        lit
          (cons item (tweak-text rest nil lit))
        caps
          (cons (char-upcase item) (tweak-text rest nil lit))
        t
          (cons (char-downcase item) (tweak-text rest nil nil))
      ))))

(defun game-print (lst)
  (bb trimmed (coerce (string-trim "() " (prin1-to-string lst)) 'list)
    (display (coerce (tweak-text trimmed t nil) 'string))
  )
  (fresh-line)
)


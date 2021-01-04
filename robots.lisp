(load "ergolib/init")
(require :ergolib)
(define-synonym add1 1+)
(define-synonym sub1 1-)
(define-synonym eq? eq)
(define-synonym every? every)
(defun below (n) (counter 0 n))

(defc board-width 64)
(defc board-height 16)
(defc board-size (* board-width board-height))

;; directions in dvorak keyboard layout
(defc directions `(
  (g . ,(- (add1 board-width))) (c . ,(- board-width)) (r . ,(- (sub1 board-width)))
  (h .                      -1)                        (n .                       1)
  (m .     ,(sub1 board-width)) (w .     ,board-width) (v .     ,(add1 board-width))
))

(defun multiple? (val sequence)
  (> (count val sequence) 1))

(defun manhattan-distance (p1 p2)
  (+ (abs (- (mod p2 board-width)
	     (mod p1 board-width)))
     (abs (- (truncate p2 board-width)
	     (truncate p1 board-width)))
  ))

(defun step-toward (aim current-pos)
  (cdar (sort (for pair in directions collect
		(bb new-pos (+ current-pos (cdr pair))
		  (cons (manhattan-distance aim new-pos)
			new-pos)
		))
	      '<
	      :key #'car
        )))

(defun read-command (pos)
  (format t "~%gcr/h n/mwv to move, (t)eleport, (l)eave:")
  (force-output)
  (bb c (read)
      d (assoc c directions)
    (mcond d (+ pos (cdr d))
	   (eq? 't c) (random board-size)
	   (eq? 'l c) nil
	   (read-command pos)
    )))

(defun print-board (pos monsters)
  (format t
	  "~%|~{~<|~%|~,65:;~A~>~}|"
	  (for i in (below board-size) collect
	    (mcond
	      (member i monsters)
		(if (multiple? i monsters) #\# #\A)
	      (= i pos) #\@
	      #\ ;
	    ))))

(defun robots ()
  (bb pos (+ (ash board-size -1) (ash board-width -1))
      monsters (loop repeat 10 collect (random board-size))
    (iterate main ( (pos pos) (monsters monsters) )
      (print-board pos monsters)
      (bb new-pos (aif (read-command pos) it (return-from main 'bye))
	(bb new-monsters (progn
	      (for mpos in monsters collect
		(if (multiple? mpos monsters)
		  mpos  ; scrap doesn't move
		  (step-toward new-pos mpos)
		)))
	  (if (every? (fn (mpos) (multiple? mpos new-monsters)) new-monsters)
	    (return-from main 'player-wins)
	    (when (member new-pos new-monsters)
	      (return 'player-loses)))
	  (main new-pos new-monsters)
	)))))

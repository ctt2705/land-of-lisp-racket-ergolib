(load "graph-util")
(define-synonym eqv? eql)
(define-synonym push! push)
(define-synonym zero? zerop)
(define-synonym equal? equal)
(define-synonym set! setf)
(define-synonym pushnew! pushnew)
(define-synonym for-each mapc)

(defv city-nodes nil)
(defv city-edges nil)
(defv visited-nodes nil)
(defv node-num 30)
(defv edge-num 45)
(defc worm-num 3)
(defc cop-odds 15)

(defun random-node ()
  (1+ (random node-num))
)

(defun edge-pair (a b)
  (unless (eqv? a b)
    (list (cons a b) (cons b a))
  ))

(defun make-edge-list ()
  (apply #'append (loop repeat edge-num collect
    (edge-pair (random-node) (random-node))
  )))

(defun direct-edges (node edge-list)
  (filter edge-list (fn (x) (eqv? (car x) node)))
)

(defun get-connected (node edge-list)
  (bb visited nil
    (iterate traverse ( (node node) )
      (unless (member node visited)
        (push! node visited)
        (for edge in (direct-edges node edge-list) do
          (traverse (cdr edge))
        )))
    visited
  ))

(defun find-islands (nodes edge-list)
  (bb islands nil
    (iterate find-island ( (nodes nodes) )
      (bb connected (get-connected (car nodes) edge-list)
          unconnected (set-difference nodes connected)
        (push! connected islands)
        (when unconnected
          (find-island unconnected)
        )))
    islands
  ))

(defun connect-with-bridges (islands)
  (when (cdr islands)
    (cat (edge-pair (caar islands) (caadr islands))
         (connect-with-bridges (cdr islands))
    )))

(defun connect-all-islands (nodes edge-list)
  (cat (connect-with-bridges (find-islands nodes edge-list))
        edge-list
  ))

(defun make-city-edges ()
  (bb nodes (force (counter 1 (1+ node-num)))
      edge-list (connect-all-islands nodes (make-edge-list))
      cops (filter edge-list (fn (x) (zero? (random cop-odds))))
    (add-cops (edges-to-alist edge-list) cops)
  ))

(defun edges-to-alist (edge-list)
  (for node1 in (remove-duplicates (mapcar #'car edge-list)) collect
    (cons node1
          (bb edges (direct-edges node1 edge-list)
            (for edge in (remove-duplicates edges :test #'equal?) collect
              (list (cdr edge))
            )))))

(defun add-cops (edge-alist edges-with-cops)
  (for x in edge-alist collect
    (bb node1 (car x)
        node1-edges (cdr x)
      (cons node1
            (for edge in node1-edges collect
              (bb node2 (car edge)
                  pairs (edge-pair node1 node2)
                (if (cl:intersection pairs edges-with-cops :test #'equal?)
                  (list node2 'cops)
                  edge
                )))))))

(defun neighbors (node edge-alist)
  (mapcar #'car (cdr (assoc node edge-alist)))
)

(defun within-one (a b edge-alist)
  (member b (neighbors a edge-alist))
)

(defun within-two (a b edge-alist)
  (or (within-one a b edge-alist)
      (some (fn (x) (within-one x b edge-alist))
            (neighbors a edge-alist))))

(defun make-city-nodes (edge-alist)
  (bb wumpus (random-node)
      glow-worms (loop for i below worm-num collect (random-node))
    (loop for n from 1 to node-num collect
      (cat (list n)
           (mcond
             (eqv? n wumpus)
               '(wumpus)
             (within-two n wumpus edge-alist)
               '(blood!)
           )
           (mcond
             (member n glow-worms)
               '(glow-worm)
             (some (fn (worm) (within-one n worm edge-alist)) glow-worms)
               '(lights!)
           )
           (when (some #'cdr (cdr (assoc n edge-alist)))
             '(sirens!)
           )))))

(defun new-game ()
  (set! city-edges (make-city-edges))
  (set! city-nodes (make-city-nodes city-edges))
  (set! player-pos (find-empty-node))
  (set! visited-nodes (list player-pos))
  (draw-city)
)

(defun find-empty-node ()
  (bb x (random-node)
    (if (cdr (assoc x city-nodes))
      (find-empty-node)
      x
    )))

(defun draw-city ()
  (ugraph->png "city" city-nodes city-edges)
)

(defun known-city-nodes ()
  (mapcar
    (fn (node)
      (if (member node visited-nodes)
        (bb n (assoc node city-nodes)
          (if (eqv? node player-pos)
            (cat n '(*))
            n
          ))
        (list node '?)
      ))
    (bb neighb-visited
        (mappend! (fn (node)
                    (mapcar #'car
                            (cdr (assoc node city-edges))))
                  visited-nodes)
      (remove-duplicates (cat visited-nodes neighb-visited))
    )))

(defun known-city-edges ()
  (mapcar
    (fn (node)
      (cons node (mapcar (fn (x)
                           (if (member (car x) visited-nodes)
                             x
                             (list (car x))
                           ))
                         (cdr (assoc node city-edges))
                 )))
    visited-nodes
  ))

(defun draw-known-city ()
  (ugraph->png "known-city"
               (known-city-nodes)
               (known-city-edges)
  ))

(defun new-game ()
  (set! city-edges (make-city-edges))
  (set! city-nodes (make-city-nodes city-edges))
  (set! player-pos (find-empty-node))
  (set! visited-nodes (list player-pos))
  (draw-city)
  (draw-known-city)
)

(defun walk (pos)
  (handle-direction pos nil)
)

(defun charge (pos)
  (handle-direction pos t)
)

(defun handle-direction (pos charging)
  (bb edge (assoc pos
                  (cdr (assoc player-pos city-edges)))
    (if edge
      (handle-new-place edge pos charging)
      (display "That location does not exist!")
    )))

(defun handle-new-place (edge pos charging)
  (bb node (assoc pos city-nodes)
      has-worm (and (member 'glow-worm node)
                    (not (member pos visited-nodes)))
    (pushnew! pos visited-nodes)
    (set! player-pos pos)
    (draw-known-city)
    (mcond
      (member 'cops edge)
        (display "You ran into the cops. Game Over.")
      (member 'wumpus node)
        (if charging
          (display "You found the Wumpus!")
          (display "You got ambushed by the Wumpus! Game Over."))
      charging
        (display "You wasted your last bullet. Game Over.")
      has-worm
        (bb new-pos (random-node)
          (display "You ran into a Glow Worm Gang! You're now at ")
          (display new-pos)
          (handle-new-place nil new-pos nil)
        ))))

;;;; Added for testing

(defc nodes-T (force (counter 1 9)))
(defc edge-list-T
  '((1 . 2) (2 . 1) (2 . 3) (3 . 2) (3 . 4) (4 . 3) (2 . 5) (5 . 2)
    (6 . 7) (7 . 6) (7 . 8) (8 . 7))
)

;;;; Ch.9 §A Faster Grand Theft Wumpus Using Hash Tables

;(set! edge-num 1000)
;(set! node-num 1000)

(defun time-get-connected ()
  (time (dotimes (i 100)
                 (get-connected 1 (make-edge-list)))))

(defun hash-edges (edge-list)
  (bb tab (make-hash-table)
    (for x in edge-list do
      (bb node (car x)
        ; ref was about 10 times as slow as gethash
        ; my guess is it's due to type dispatch
        (push! (cdr x) (gethash node tab))
      ))
    tab
  ))

(defun get-connected-hash (node edge-tab)
  (bb visited (make-hash-table)
    (iterate traverse ( (node node) )
      (unless (gethash node visited)
        (set! (gethash node visited) t)
        (for-each #'traverse (gethash node edge-tab))
      ))
    visited
 ))

(defun time-get-connected-hash ()
  (time (dotimes (i 100)
                 (get-connected-hash 1 (hash-edges (make-edge-list))))))


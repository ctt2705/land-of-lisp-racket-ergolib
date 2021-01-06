(load "ergolib/init")
(require :ergolib)
(defun below (n) (counter 0 n))
(define-synonym add1 1+)
(define-synonym sub1 1-)
(define-synonym display princ)
(define-synonym eq? eq)
(define-synonym zero? zerop)
(defmethod ->vector ((lst list))
  (make-array (length lst) :initial-contents lst))
(define-synonym setref! setref)
(define-synonym set! setf)
(setf $default-dictionary-impl-type 'eql-hash-table)

(defc $num-players 2)
(defc $max-dice 3)
(defc $board-size 3)
(defc $board-hexnum (* $board-size $board-size))

(defun gen-board ()
  (for n in (below $board-hexnum) vcollect
    (cons (random $num-players) (add1 (random $max-dice)))
  ))

(defun player-letter (n)
  (code-char (+ 97 n)))

(defun draw-board (board)
  (for y in (below $board-size) do
    (fresh-line)
    (bb num-space (* 2 (- $board-size y))
      (format t (cat "~,," (->string num-space) "@a") ""))
    (for x in (below $board-size) do
      (bb hex (ref board (+ x (* $board-size y)))
        (format t "~a-~a "
                  (player-letter (car hex))
                  (cdr hex))
      ))))

(defun game-tree (board player spare-dice first-move?)
  (bb state (cons board player)
      branches (cat (passing-move board player spare-dice first-move?)
                    (attacking-moves board player spare-dice))
                    
    (cons state branches)
  ))

(define-synonym board-of caar)
(define-synonym player-of cdar)
(define-synonym branches-of cdr)

(defun next-player (player)
  (mod (add1 player) $num-players))

(defun passing-move (board player spare-dice first-move?)
  (if first-move?
    nil
    (list (cons nil
                (game-tree (add-new-dice board player (sub1 spare-dice))
                           (next-player player)
                           0
                           t)))
  ))

(defun attacking-moves (board cur-player spare-dice)
  (bb :fn player (pos) (car (ref board pos))
      :fn dice (pos) (cdr (ref board pos))
      :fn cur-player? (pos) (= (player pos) cur-player)
      hexes-cur-player (filter (below $board-hexnum) #'cur-player?)
      :fn legal-attack? (src dst) (and (not (cur-player? dst))
                                       (> (dice src) (dice dst)))
      :fn legal-neighbors (src)
            (for dst in (neighbors src) if (legal-attack? src dst) collect
              (cons src dst))
      legal-attacks (mappend! #'legal-neighbors hexes-cur-player)
    (for move in legal-attacks collect
      (bb src (car move) dst (cdr move)
        (cons move
              (game-tree (board-attack board cur-player src dst (dice src))
                         cur-player
                         (+ spare-dice (dice dst))
                         nil))
      ))))

(defun neighbors (pos)
  (bb up (- pos $board-size)
      down (+ pos $board-size)
      left-edge? (zero? (mod pos $board-size))
      right-edge? (zero? (mod (add1 pos) $board-size))
      candidates (cat
        (list up down)
        (unless left-edge? (list (sub1 up) (sub1 pos)))
        (unless right-edge? (list (add1 pos) (add1 down)))
      )
      :fn inside? (p) (and (>= p 0) (< p $board-hexnum))
    (filter candidates #'inside?)
  ))

(defun board-attack (board player src dst dice)
  (for (hex pos) in (zip board (counter)) vcollect
    (mcond
      (eq? pos src) (cons player 1)
      (eq? pos dst) (cons player (sub1 dice))
      hex
    )))


(defun add-new-dice (board player spare-dice)
  (->vector (iterate f ( (lst (coerce board 'list))
                         (n spare-dice)
                         (acc nil) )
    (mcond
      (zero? n) (cat (reverse acc) lst)
      (not lst) (reverse acc)
      (bb cur-player (caar lst)
          cur-dice (cdar lst)
        (if (and (eq? cur-player player)
                 (< cur-dice $max-dice))
          (f (cdr lst)
             (sub1 n)
             (cons (cons cur-player (add1 cur-dice)) acc) )
          (f (cdr lst) n (cons (car lst) acc) )
        ))))))

(defun play-vs-human (tree)
  (print-info tree)
  (if (branches-of tree)
    (play-vs-human (handle-human tree))
    (announce-winner (board-of tree))
  ))

(defun print-info (tree)
  (fresh-line)
  (format t "current player = ~a" (player-letter (player-of tree)))
  (draw-board (board-of tree))
)

(defun handle-human (tree)
  (fresh-line)
  (display "choose your move:")
  (for (branch n) in (zip (branches-of tree) (counter 1)) do
    (fresh-line)
    (format t "~a. " n)
    (bb move (car branch)
      (if move
        (format t "~a -> ~a" (car move) (cdr move))
        (display "end turn")
      )))
  (fresh-line)
  (cdr (ref (branches-of tree) (sub1 (read))))
)

(defun winners (board)
  (bb tally (for hex in board collect (car hex))
      totals (for player in (remove-duplicates tally) collect
               (cons player (count player tally)))
      best (apply #'max (mapcar #'cdr totals))
    (mapcar #'car
            (filter totals (fn (x) (eq? (cdr x) best))))
  ))

(defun announce-winner (board)
  (fresh-line)
  (bb w (winners board)
    (if (> (length w) 1)
      (format t "The game is a tie between ~a"
                (mapcar #'player-letter w))
      (format t "The winner is ~a"
                (player-letter (car w)))
    )))

(defun rate-position (tree player)
  (bb branches (branches-of tree)
    (if branches
      (apply (if (eq? (player-of tree) player) #'max #'min)
             (get-ratings tree player))
      (bb w (winners (board-of tree))
        (if (member player w)
          (/ 1 (length w))
          0
        )))))

(defun get-ratings (tree player)
  (for branch in (branches-of tree) collect
    (rate-position (cdr branch) player)
  ))

(defun handle-computer (tree)
  (bb ratings (get-ratings tree (player-of tree))
    (cdr (ref (branches-of tree)
              (position (apply #'max ratings) ratings)))
  ))

(defun play-vs-computer (tree)
  (print-info tree)
  (mcond
    (not (branches-of tree))
      (announce-winner (board-of tree))
    (zero? (player-of tree))
      (play-vs-computer (handle-human tree))
    (play-vs-computer (handle-computer tree))
  ))

(bb old-neighbors (symbol-function 'neighbors)
    previous (dict)
  (defun neighbors (pos)
    (or (ref1 previous pos)
        (setref! previous pos (funcall old-neighbors pos)))
  ))

(bb old-game-tree (symbol-function 'game-tree)
    previous (make-dictionary-implementation 'equalp-hash-table)
  (defun game-tree (&rest rest)
    (or (ref1 previous rest)
        (setref! previous rest (apply old-game-tree rest)))
  ))

(bb old-rate-position (symbol-function 'rate-position)
    previous (dict)
  (defun rate-position (tree player)
    (bb tab (ref1 previous player)
      (unless tab
        (set! tab (setref! previous player (dict))))
      (or (ref1 tab tree)
          (setref! tab tree
                       (funcall old-rate-position tree player)))
    )))

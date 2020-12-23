(load "ergolib/init")
(require :ergolib)
(define-synonym add! add)
(define-synonym set! setf)
(define-synonym dec! decf)
(define-synonym inc! incf)
(define-synonym display princ)
(define-synonym equal? equal)
(define-synonym push! push)
(define-synonym zero? zerop)
(define-synonym del! del)

(defc width 100)
(defc height 30)
(defc jungle '(45 10 10 10))
(defc plant-energy 80)

(defv plants (make-set))

(defun random-plant (left top width height)
  (bb pos (cons (+ left (random width)) (+ top (random height)))
    (add! plants pos)
  ))

(defun add-plants ()
  (apply #'random-plant jungle)
  (random-plant 0 0 width height)
)

(defstruct animal x y energy dir genes)

(defv animals
  (list (make-animal :x      (ash width -1)
                     :y      (ash height -1)
                     :energy 1000
                     :dir    0
                     :genes  (loop repeat 8 collecting
                               (1+ (random 10))
  ))))

(define-method (move! (_ animal dir x y energy)) 
  (set! x 
    (mod (+ x
            (mcond
              (and (>= dir 2) (< dir 5)) 1
              (or (= dir 1) (= dir 5)) 0
              -1)
            width)
         width))
  (set! y
    (mod (+ y
            (mcond
              (and (>= dir 0) (< dir 3)) -1
              (and (>= dir 4) (< dir 7)) 1
              0)
            height)
         height))
  (dec! energy)
)

(define-method (turn! (_ animal dir genes))
  (bb x (random (apply #'+ genes))
    (bb :fn angle (genes x)
              (bb xnu (- x (car genes))
                (if (< xnu 0)
                  0
                  (1+ (angle (cdr genes) xnu))
                ))
      (set! dir (mod (+ dir (angle genes x)) 8))
    )))

(define-method (eat! (_ animal x y energy))
  (bb pos (cons x y)
    (when (member? plants pos)
      (inc! energy plant-energy)
      (del! plants pos)
    )))

(defc reproduction-energy 200)

(define-method (reproduce! (animal animal energy genes))
  (when (>= energy reproduction-energy)
    (set! energy (ash energy -1))
    (bb animal-nu (copy-structure animal)
        genes     (copy-list genes)
        mutation  (random 8)
      (set! (ref genes mutation)
        (max 1 (+ (ref genes mutation) (random 3) -1)))
      (set! (animal-genes animal-nu) genes)
      (push! animal-nu animals)
    )))

(defun update-world ()
  (set! animals (remove-if (fn (animal)
                             (<= (animal-energy animal) 0))
                           animals))
  (for animal in animals do
    (turn! animal)
    (move! animal)
    (eat! animal)
    (reproduce! animal)
  )

  (add-plants)
)

(defun draw-world ()
  (loop for y below height do (progn
    (fresh-line)
    (display "|")
    (loop for x below width do
      (display (mcond
                 (some (fn (animal)
                         (and (= (animal-x animal) x)
                              (= (animal-y animal) y)))
                       animals)
                   #\M
                 (member? plants (cons x y))
                   #\*
                 #\space)))
      (display "|")
    )))

(defun evolution ()
  (draw-world)
  (fresh-line)
  (bb str (read-line)
    (if (equal? str "quit")
      ()
      (bb x (parse-integer str :junk-allowed t)
        (if x
          (loop for i below x do
            (update-world)
            if (zero? (mod i 1000)) do
              (display #\.))
          (update-world)
        )
        (evolution)
      ))))


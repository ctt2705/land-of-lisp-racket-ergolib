#lang racket
(require "graph-util.rkt")

(define *city-nodes* #f)
(define *city-edges* #f)
(define *visited-nodes* #f)
(define *node-num* 30)
(define *edge-num* 45)
(define *worm-num* 3)
(define *cop-odds* 15)

(define (random-node)
  (add1 (random *node-num*))
)

(define (edge-pair a b)
  (if (eqv? a b)
    null ; necessary for append in calling function
    (list (cons a b) (cons b a))
  ))

(define (make-edge-list)
  (apply append (for/list ([_ *edge-num*])
                  (edge-pair (random-node) (random-node))
  )))

(define (direct-edges node edge-list)
  (filter (lambda (x)
            (eqv? (car x) node))
          edge-list
  ))

;; Emulates CL push (only works for list)—same as in wizards-game.rkt
(define-syntax push!
  (syntax-rules ()
    [(push! elem lst) (set! lst (cons elem lst))]
  ))

(define (get-connected node edge-list)
  (let ([visited '()])
    (let traverse ([node node])
      (unless (member node visited)
        (push! node visited)
        (for-each (lambda (edge) (traverse (cdr edge)))
                  (direct-edges node edge-list)
        )))
    visited
  ))

(require racket/set)

(define (find-islands nodes edge-list)
  (let ([islands '()])
    (let find-island ([nodes nodes])
      (let* ([connected
               (get-connected (car nodes) edge-list)]
             [unconnected
               (set-subtract nodes connected)])
        (push! connected islands)
        (unless (empty? unconnected)
          (find-island unconnected)
        )))
    islands
  ))

(define (connect-with-bridges islands)
  (if (empty? (cdr islands))
    null
    (append (edge-pair (caar islands) (caadr islands))
            (connect-with-bridges (cdr islands)))
  ))

(define (connect-all-islands nodes edge-list)
  (append (connect-with-bridges (find-islands nodes edge-list))
          edge-list)
)

(define (make-city-edges)
  (let* ([nodes (for/list ([i *node-num*]) (add1 i))]
         [edge-list (connect-all-islands nodes (make-edge-list))]
         [cops (filter (lambda (_) (zero? (random *cop-odds*)))
                       edge-list)])
    (add-cops (edges-to-alist edge-list) cops)
  ))

(define (edges-to-alist edge-list)
  (map (lambda (node1)
         (cons node1
               (map (lambda (edge)
                         (list (cdr edge)))
                    (remove-duplicates (direct-edges node1 edge-list))
               )))
       (remove-duplicates (map car edge-list) eqv?)
  ))

(define (add-cops edge-alist edges-with-cops)
  (map (lambda (x)
         (let ([node1 (car x)]
               [node1-edges (cdr x)])
           (cons node1
                 (map (lambda (edge)
                        (let ([node2 (car edge)])
                          (if (empty? (set-intersect (edge-pair node1 node2)
                                                     edges-with-cops))
                            edge
                            (list node2 'cops)
                          )))
                      node1-edges
                 ))))
       edge-alist
  ))

(define (neighbors node edge-alist)
  (map car (cdr (assoc node edge-alist)))
)

(define (within-one? a b edge-alist)
  (member b (neighbors a edge-alist))
)

(define (within-two? a b edge-alist)
  (or (within-one? a b edge-alist)
      (for/or ([x (neighbors a edge-alist)])
        (within-one? x b edge-alist)
      )))

(define (make-city-nodes edge-alist)
  (let ([wumpus (random-node)]
        [glow-worms (for/list ([_ *worm-num*]) (random-node))])
    (for/list ([n (range 1 (add1 *node-num*))])
      (append (list n)
              (cond
                [(eqv? n wumpus)
                  '(wumpus)]
                [(within-two? n wumpus edge-alist)
                  '(blood!)]
                [else null])
              (cond
                [(member n glow-worms)
                  '(glow-worm)]
                [(for/or ([worm glow-worms]) (within-one? n worm edge-alist))
                  '(lights!)]
                [else null])
              (if (for/or ([x (cdr (assoc n edge-alist))])
                    (not (empty? (cdr x))))
                '(sirens!)
                null
              )))))

(define *player-pos* null) ; missing in CL version

(define (new-game)
  (set! *city-edges* (make-city-edges))
  (set! *city-nodes* (make-city-nodes *city-edges*))
  (set! *player-pos* (find-empty-node))
  (set! *visited-nodes* (list *player-pos*))
  (draw-city)
  (draw-known-city) ; added later in the same chapter
)

(define (find-empty-node)
  (let ([x (random-node)])
    (if (empty? (cdr (assoc x *city-nodes*)))
      x
      (find-empty-node)
    )))

(define (draw-city)
  (ugraph->png "city" *city-nodes* *city-edges*)
)

(define (known-city-nodes)
  (map (lambda (node)
         (if (member node *visited-nodes*)
           (let ([n (assoc node *city-nodes*)])
             (if (eqv? node *player-pos*)
               (append n '(*))
               n
             ))
           (list node '?)
         ))
       (remove-duplicates
         (append *visited-nodes*
                 (append-map (lambda (node)
                               (map car
                                    (cdr (assoc node *city-edges*))
                               ))
                             *visited-nodes*
                 )))))

(define (known-city-edges)
  (map (lambda (node)
         (cons node (map (lambda (x)
                           (if (member (car x) *visited-nodes*)
                             x
                             (list (car x))
                           ))
                         (cdr (assoc node *city-edges*))
                    )))
       *visited-nodes*
  ))

(define (draw-known-city)
  (ugraph->png "known-city" (known-city-nodes) (known-city-edges))
)

(define (walk pos)
  (handle-direction pos #f)
)

(define (charge pos)
  (handle-direction pos #t)
)

(define (handle-direction pos charging)
  (let ([edge (or
                (assoc pos (cdr (assoc *player-pos* *city-edges*)))
                null
              )])
    (if (empty? edge)
      (display "That location does not exist!")
      (handle-new-place edge pos charging)
    )))

;; Emulates CL pushnew
(define-syntax pushnew!
  (syntax-rules ()
    [(pushnew! elem lst) (unless (member elem lst) (set! lst (cons elem lst)))]
  ))

(define (handle-new-place edge pos charging)
  (let* ([node (assoc pos *city-nodes*)]
         [has-worm (and (member 'glow-worm node)
                        (not (member pos *visited-nodes*)))])
    (pushnew! pos *visited-nodes*)
    (set! *player-pos* pos)
    (draw-known-city)
    (cond
      [(member 'cops edge)
        (display "You ran into the cops. Game Over.")]
      [(member 'wumpus node)
        (if charging
          (display "You found the Wumpus!")
          (display "You got ambushed by the Wumpus. Game Over.")
        )]
      [charging
        (display "You wasted your last bullet. Game over.")]
      [has-worm
        (let ([new-pos (random-node)])
          (display "You ran into a Glow Worm Gang! You're now at ")
          (display new-pos)
          (handle-new-place '() new-pos #f)
        )])))

;;;; Added for testing

(define *nodes-T* (for/list ([n 8]) (add1 n)))
(define *edge-list-T*
  '((1 . 2) (2 . 1) (2 . 3) (3 . 2) (3 . 4) (4 . 3) (2 . 5) (5 . 2)
    (6 . 7) (7 . 6) (7 . 8) (8 . 7))
)

;;;; Ch.9 §A Faster Grand Theft Wumpus Using Hash Tables

;(set! *edge-num* 1000)
;(set! *node-num* 1000)

(define (time-get-connected)
  (time (for ([_ 100]) (get-connected 1 (make-edge-list))))
)

(define (hash-edges edge-list)
  (let ([tab (make-hash)])
    (for-each (lambda (x)
                (let ([node (car x)])
                  (hash-set! tab
                             node
                             (cons (cdr x)
                                   (hash-ref tab node empty)))
                ))
              edge-list)
    tab
  ))

(define (get-connected-hash node edge-tab)
  (let ([visited (make-hash)])
    (let traverse ([node node])
      (unless (hash-ref visited node #f)
        (hash-set! visited node #t)
        (for-each (lambda (edge) (traverse edge))
                  (hash-ref edge-tab node empty)
        )))
    visited
  ))

(define (time-get-connected-hash)
  (time (for ([_ 100])
          (get-connected-hash 1 (hash-edges (make-edge-list)))))
)

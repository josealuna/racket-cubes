#lang racket
;; importing for using.
;; threading for piping operator.
;; rackunit for testing.
;; 09.03.2022 -- Finishing the searching
(require threading rackunit dyoo-while-loop data/queue "state_structure.rkt" "cube_structure.rkt")



;; All posibilities of changing the state of the cubes
;; Full all cubes
;; Empty all cubes
;; Transbase from one to another.

;; Given two cubes make a transbase from one to another.
;; Cube -> Cube -> (Pair Cube Cube)
(define (transbase from to)
  (let* ([cap1 (Cube-capacity from)]
         [cap2 (Cube-capacity to)]
         [con1 (Cube-content from)]
         [con2 (Cube-content to)]
         [receiving (min (- cap2 con2) con1)]
         [new-con1 (- con1 receiving)]
         [new-con2 (+ con2 receiving)])
    (list (Cube cap1 new-con1) (Cube cap2 new-con2))))
;; module test
(module+  test
  (check-equal?  (transbase (Cube 4 1) (Cube 3 0)) (list (Cube 4 0) (Cube 3 1)))
  (check-equal?  (transbase (Cube 4 1) (Cube 3 2)) (list (Cube 4 0) (Cube 3 3)))
  (check-equal?  (transbase (Cube 4 3) (Cube 3 2)) (list (Cube 4 2) (Cube 3 3)))
  (check-equal?  (transbase (Cube 4 1) (Cube 3 1)) (list (Cube 4 0) (Cube 3 2)))
  (check-equal?  (transbase (Cube 4 0) (Cube 3 2)) (list (Cube 4 0) (Cube 3 2)))
  (check-equal?  (transbase (Cube 4 2) (Cube 3 0)) (list (Cube 4 0) (Cube 3 2)))
  )
;; empty
;; Cube -> Cube
(define (empting cube)
   (let ([cap1 (Cube-capacity cube)])
     (Cube cap1 0)))

;; fullness
;; Cube -> Cube
(define (filling cube)
  (let ([cap1 (Cube-capacity cube)])
    (Cube cap1 cap1)))
;; function for ordering the cubes
(define order  (λ (cube1 cube2)
          (> (Cube-capacity cube1)
             (Cube-capacity cube2))))


;; list of cubes -> function -> (listofcubes -> listofcubes)
(define (apply-to-cubes function)
  ;; function to be used
  (λ (listofcubes)
  (sort (cons (function (car listofcubes))
              (rest listofcubes))
        order)))
       
;; Applying transbase to all the 
(define (apply-two-cubes function)
  ;; function
  (λ (listofcubes)
    (let* ([from (car listofcubes)]
           [r    (cdr listofcubes)]
           [to   (car r)]
           [rest (rest r)])
    (sort (append (function from to) rest) order))))


;; state -> list of cubes
(define (state->listofcubes state)
    ;; auxiliar for applying the maps
  (define (f-aux per f1 function)
    (~>> per
         (map (f1 function))))
  ;; body function
  (let* ([cubes (State-cubes state)]
         [lofcubes (Cubes-cubes cubes)]
         [g (Cubes-goal cubes)]
         [per (permutations lofcubes)]
         [from-fullfit  (f-aux per apply-to-cubes filling)]
         [from-empting  (f-aux per apply-to-cubes empting)]
         [from-trans    (f-aux per apply-two-cubes transbase)])
 (~>>
  (append from-empting  from-fullfit from-trans)
  (map (λ (cubes) (Cubes cubes g))))))


;; state -> setofcubes -> list of cubes filtered
;; Only consider the Cubes that are not visited.
(define (state->filtered set state)
  (~>> state
       state->listofcubes
       (filter (λ (cube) (not (set-member? set cube))))))

;; list of cubes genera all states from all combinations
;; We only need to change one of them
;; Cubes (list (Cube 4 0)  (Cube 3 0)) 1)
(define (state->lofstates state)
  (~>> state
       state->listofcubes
       (map (λ (loc) (State loc state)))))


;; Now we need to create a bucle for looking for but considering
;; we must avoid the Cubes we have visited.



;; (state->lofstates (State lista-1 'None))

;; (~>> (list 1 2 3)
;;     permutations
;;     (map (λ (l) (take l 2))))

;; -----------------------------------------------------------------------------------------------------------------------
;; ------------------------------------------------- searching part ------------------------------------------------------
;; -----------------------------------------------------------------------------------------------------------------------
;; Discover if a Cubes is a solution
;; Cubes -> boolean
(define (is-cube-solution? cubes)
  (let ([goal (Cubes-goal cubes)])
         (~>>
          (Cubes-cubes cubes)
          (map Cube-content)
          (ormap (λ (x) (equal? x goal))))))

(module+ test
  ; true propositions
  (check-true   (ormap (λ (x) (equal? x 2)) (list 1 2 3)))
  (check-true   (is-cube-solution? (Cubes (list (Cube 4 0) (Cube 3 2)) 2)))
  (check-true   (is-cube-solution? (Cubes (list (Cube 4 0) (Cube 3 2)) 0)))
  (check-true   (is-cube-solution? (Cubes (list (Cube 4 4) (Cube 3 2)) 4)))
  ; false propositions
  (check-false  (is-cube-solution? (Cubes (list (Cube 4 0) (Cube 3 2)) 3)))
  (check-false  (ormap (λ (x) (equal? x 4)) (list 1 2 3))))

;; Discover if a State is a solution
;; State -> boolean
(define (is-state-solution? state)
  (let ([cubes (State-cubes state)])
    (is-cube-solution? cubes)))
(module+ test
  (define base      (State (Cubes (list (Cube 4 2) (Cube 3 0)) 1) 'None))
  (define example-1 (State (Cubes (list (Cube 4 2) (Cube 3 0)) 1) base))
  (define example-2 (State (Cubes (list (Cube 4 2) (Cube 3 0)) 2) base))
  ;; true statements tests
  (check-true  (is-state-solution? example-2))
  (check-false (is-state-solution? example-1))
  ;; false statements tests
  (check-false (is-state-solution? base)))
                          
  
;; searching from a initial state to find a solution
;; state -> state which is solution
;; this functin find the first solution from the space of solutions.
;; Does not find all the solutions. 
(define (search initial-state [max 100])
  (let* ([num-iter       0]
         [visited       (mutable-set)]     ;; record the visited states
         [initial-cubes
          (State-cubes initial-state)]     ;; initial cubes
         [queue-states  (make-queue)]      ;; queue for the states
         [solution 'None])                 ;; solution
    (set-add! visited initial-cubes)       ;; visited            
    (enqueue! queue-states initial-state)  ;; queue for the states
    (while
     (not (queue-empty? queue-states))     ;; while not empty the queue
     ;; operating
     (let*
         ([state  (dequeue! queue-states)])
       (cond
         [(> num-iter max) (break)]        ;; if we have grat number of iterations.
         [(is-state-solution? state)       ;; we have a solution
          (set! solution state)            ;; set the solution
          (break)]                         ;; break
         [else
          (let* ([next-cubes (state->filtered visited state)]
                 [next-states (~>>
                               next-cubes
                               (map (λ (cubes) (State cubes state))))])

            (set! num-iter (+ 1 num-iter))
            (for-each (λ (cubes)  (set-add! visited cubes)) next-cubes)
            (for-each (λ (state)  (enqueue! queue-states state)) next-states)
            (continue))])))
    solution))
;; some simple tests
(module+ test
  ;; (search base 20)                 ;; we can decide the number of iterations 
  ;; (state->list  (search base 20)) ;; we
  ;; look for another solution
  (define base-2
    (State (Cubes (list (Cube 5 0) (Cube 11 0) (Cube 3 0)) 7) 'None))
  (state->list  (search base-2 200))) ;; we



;; We just need a loop for the actualizing the visited until we reach the solution or we reach certain number of interations.

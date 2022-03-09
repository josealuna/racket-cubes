#lang racket
;; importing for using.
;; threading for piping operator.
;; rackunit for testing.
;; 09.03.2022 -- Finishing the searching
(require threading rackunit dyoo-while-loop data/queue  ;; basic importings
         "state_structure.rkt" "cube_structure.rkt")    ;; structures

                 
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
         [(> num-iter max) (break)]        ;; if we have greater number of iterations.
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
  (define base-2
    (State (Cubes (list (Cube 5 0) (Cube 11 0) (Cube 3 0)) 2) 'None))
  (state->list  (search base-2 200))) ;; we

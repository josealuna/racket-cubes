#lang racket

(require threading rackunit "cube_structure.rkt")

(provide State State-cubes State-states
         state->list
         state->listofcubes state->filtered state->lofstates 
         is-state-solution?)


;; State cubes reflecting the state of the cubes
;; From a state we get states
;; TODO we can include a field for storing the way this state is obtained
(struct State (cubes  ;; Cubes                   --> (Cubes (list (Cube 4 0)  (Cube 3 0)) 1))
               states ;; Recursive form of state --> (State (Cubes (list (Cube 4 0) (Cube 3 0)) 1) 'None))
               )      #:transparent)


;; Passing from a State to a list of cubes
;; State -> list of Cubes
(define (state->list state [acc '()])
  (cond
    [(equal? state 'None) acc]  ;; Base case
    [else
     (let ([cubes (State-cubes state)]
           [next  (State-states state)])
       (state->list next (cons cubes acc)))]))


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
                          



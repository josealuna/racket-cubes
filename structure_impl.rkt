#lang racket

(require threading rackunit "structures.rkt")

(provide transbase empting filling filling apply-to-cubes apply-two-cubes state->listofcubes
         state->filtered state->lofstates is-cube-solution? is-state-solution?)








;; Now we need to create a bucle for looking for but considering
;; we must avoid the Cubes we have visited.



;; (state->lofstates (State lista-1 'None))

;; (~>> (list 1 2 3)
;;     permutations
;;     (map (λ (l) (take l 2))))

;; -----------------------------------------------------------------------------------------------------------------------
;; ------------------------------------------------- searching part ------------------------------------------------------
;; -----------------------------------------------------------------------------------------------------------------------




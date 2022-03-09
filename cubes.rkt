#lang racket
;; importing for using.
;; threading for piping operator.
;; rackunit for testing.
;; 09.03.2022 -- Finishing the searching
(require threading rackunit)
;; testing threading
(~>> (list 1 2 3)
     (map (λ (x)
            (+ x 1))))
;; Creamos structures para utilizar
;; Cube  --> tiene una capacidad y un contenido.
;; Cubes --> tiene una lista de cubos y un objetivo
(struct Cube  (capacity content)  #:transparent)
(struct Cubes (cubes        ;; list of Cube
               goal         ;; objetive
               )        #:transparent)

;; Examples of cubos sencillos
;; ---> con una capacidad y un contenido. 
(define cube-0 (Cube 4 2))
(define cube-1 (Cube 3 0))

;; Examples de listas de cubos.
(define lista-1 (Cubes (list (Cube 4 2)  (Cube 3 0)) 1))


;; State cubes reflecting the state of the cubes
;; From a state we get states 
(struct State (cubes  ;; Cubes                   --> (Cubes (list (Cube 4 0)  (Cube 3 0)) 1))
               states ;; Recursive form of state --> (State (Cubes (list (Cube 4 0) (Cube 3 0)) 1) 'None))
               )      #:transparent)

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

;; list of cubes -> function -> (listofcubes -> listofcubes)
(define (apply-to-cubes function)
  (define order  (λ (cube1 cube2)
          (> (Cube-capacity cube1)
             (Cube-capacity cube2))))
  (λ (listofcubes)
  (sort (cons (function (car listofcubes))
              (rest listofcubes))
        order)))
       
;; Applying transbase to all the 
(define (apply-two-cubes function)
  ;; auxiliar
  (define order (λ (cube1 cube2)
          (> (Cube-capacity cube1)
             (Cube-capacity cube2))))
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



(state->lofstates (State lista-1 'None))

;; (~>> (list 1 2 3)
;;     permutations
;;     (map (λ (l) (take l 2))))



;; Examples of State
;; In this way we can filter just the ones in which we are interested for. 
(define visited (mutable-set))
;; podemos meter un Cubes en un set
(set-add! visited    (Cubes (list (Cube 4 0) (Cube 3 0)) 1))
(set-member? visited (Cubes (list (Cube 4 0) (Cube 3 0)) 1))
;; ((set-add!

(state->filtered  visited (State lista-1 'None))

;; We just need a loop for the actualizing the visited until we reach the solution or we reach certain number of interations.

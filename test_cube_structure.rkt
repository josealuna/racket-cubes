#lang racket
(require rackunit/gui rackunit "cube_structure.rkt")

;; basic test
(test/gui
 (test-suite
  "cubes tests"
  (test-suite
   "Basic cubes transbase function "
   (test-case "basic transbase " (check-equal?  (transbase (Cube 4 1) (Cube 3 0)) (list (Cube 4 0) (Cube 3 1))))
   (test-case "basic transbase" (check-equal?  (transbase (Cube 4 1) (Cube 3 0)) (list (Cube 4 0) (Cube 3 1))))
   (test-case "to non empty" (check-equal?  (transbase (Cube 4 1) (Cube 3 2)) (list (Cube 4 0) (Cube 3 3))))
   (test-case "respect capacity"(check-equal?   (transbase (Cube 4 3) (Cube 3 2)) (list (Cube 4 2) (Cube 3 3))))
   (test-case "to non empty limited" (check-equal?  (transbase (Cube 4 1) (Cube 3 1)) (list (Cube 4 0) (Cube 3 2))))
   (test-case "no transbase possible" (check-equal?  (transbase (Cube 4 0) (Cube 3 2)) (list (Cube 4 0) (Cube 3 2))))
   (test-case "complete transbase" (check-equal?  (transbase (Cube 4 2) (Cube 3 0)) (list (Cube 4 0) (Cube 3 2)))))
  (test-suite
   "Test cube is solution "
   (test-case "ormap works as expected-1" (check-true   (ormap (λ (x) (equal? x 2)) (list 1 2 3))))
   (test-case "In the second cube" (check-true   (is-cube-solution? (Cubes (list (Cube 4 0) (Cube 3 2)) 2))))
   (test-case "Trivial solution"   (check-true   (is-cube-solution? (Cubes (list (Cube 4 0) (Cube 3 2)) 0))))
   (test-case "First cube solution"    (check-true   (is-cube-solution? (Cubes (list (Cube 4 4) (Cube 3 2)) 4))))
   ;; false statements
   (test-case "no solution" (check-false  (is-cube-solution? (Cubes (list (Cube 4 0) (Cube 3 2)) 3))))
   (test-case "ormap works as expected-2" (check-false  (ormap (λ (x) (equal? x 4)) (list 1 2 3))))
   
   )))


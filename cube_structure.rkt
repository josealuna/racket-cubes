#lang racket

(require threading rackunit)

(provide  Cube Cube-capacity Cube-content             ;; Cube structure and associated
          Cubes Cubes-cubes Cubes-goal                ;; Cubes structure and associated
          transbase empting filling filling           ;; change in the cubes
          apply-to-cubes apply-two-cubes
          is-cube-solution? )
          
;; Creamos structures para utilizar
;; Cube  --> tiene una capacidad y un contenido.
;; Cubes --> tiene una lista de cubos y un objetivo
(struct Cube  (capacity content)  #:transparent)
(struct Cubes (cubes        ;; list of Cube
               goal         ;; objetive
               )        #:transparent)

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


;; Discover if a Cubes is a solution
;; Cubes -> boolean
(define (is-cube-solution? cubes)
  (let ([goal (Cubes-goal cubes)])
         (~>>
          (Cubes-cubes cubes)
          (map Cube-content)
          (ormap (λ (x) (equal? x goal))))))




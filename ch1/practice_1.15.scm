#lang sicp

(define (cube x) (* x x x))

(define (p x) (- (* 3 x) (* 4 (cube x))))

(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

;; a. (sine 12.15) p appears  6 times
(sine 12.15) ;Value: -.39980345741334

(p (sine 4.05))

(p (p (sine 1.3499999999999999)))

(p (p (p (sine 0.44999999999999996))))

(p (p (p (p (sine 0.15)))))

(p (p (p (p (p (sine 0.05))))))

;; b. What is the order of growth in space and number of steps (as a function of a) used by the process generated by the sine procefure when (sine a) is evaluated?
;; size of step and space is same.
;; and the size should satisfy
;; `a / 3^x > 0.1 `
;; => x = Θ(log3a)


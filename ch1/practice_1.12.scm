#lang sicp

;; Pascals' triangle


(define (pascal i j)
  (cond ((or (< i 1) (< j 1)) 0)
        ((or (= i j) (= 1 j)) 1)
        (else (+ (pascal (- i 1) (- j 1)) (pascal (- i 1) j)))))
        

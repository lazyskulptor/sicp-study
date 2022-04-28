#lang sicp

;; 에러가 나고 있음 수정 필요. 
;TODO: 답이 맞는지 확인 필요.
(define (cube-root-iter guess x) 
  (if (good-enough? guess x)
    guess
    (cube-root-iter (improve guess x) x)
  )
)

(define (good-enough? guess new-guess)
  (< (abs (- guess new-guess)) (* new-guess 0.001)))

(define (improve guess x)
  (/ 3 (+ (/ y (* x x)) (* 2 y)))
)

(define (cube-root x)
  (cube-root-iter 1.0 x)
)

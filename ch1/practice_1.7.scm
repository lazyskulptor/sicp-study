#lang sicp

;;; 작은 숫자의 경우
;
; good-enough? 에 입력 되어있는 0.001 보다 작은 숫자의 제곱근은 찾지 못함.
; 

(define (sqrt-iter guess x)
  (if (good-enough? guess (improve guess x))
      guess
      (sqrt-iter (improve guess x) x)
  )
)

;; 큰 숫자의 경우 차이가 0.001 보다 작아지지 않음.


; 개선된 프로시져. 
; 

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess new-guess)
  (< (abs (- guess new-guess)) (* new-guess 0.001)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (square x) (* x x))

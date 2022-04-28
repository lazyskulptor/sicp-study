#lang sicp
;;; practice 1.1
10 

(+ 5 3 4)

(- 9 1)

(/ 6 2)

(+ (* 2 4) (- 4 6))

(define a 3)

(define b (+ a 1))

(+ a b (* a b))

(= a b)

(if (and (> b a) (< b (* a b)))
    b
    a)

(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))

(+ 2 (if (> b a) b a))

(* (cond ((> a b) a)
         ((< a b) b)
	 (else -1))
   (+ a 1))


;;; practice 1.2 

(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))

; practice 1.3 
(define (square x) (* x x))
(define (sum-of-square x y) (+ (square x) (square y)))
(define (op1 a b c) (if (or (> a b) (= a b))
			(sum-of-square a (if (or (> b c) (= b c)) b c))
			(sum-of-square b (if (or (> b c) (= b c)) a c))))
       
;;; practice 1.4 

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
; (if (> b 0)  식이 연산자 자리에서 연산자의 역할을 하고 있음


;;; practice 1.5 

(define (p) (p))

(define (test x y)
  (if (= x 0)
    0
    y))

(test 0 (p))

; 인자 먼저 계산범(applicative order evaluation) 이라면 무한 루프에 빠지고
; 정의 대로 계산법(normal-order evaluation) 이라면 0 이 나온다
; Scheme 실행기는 인자 먼저 계산법을 사용하기 떄문에 무한 루프에 빠진다

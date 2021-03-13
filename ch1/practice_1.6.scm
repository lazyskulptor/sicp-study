#lang sicp

;;; practice 1.6

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(new-if (= 2 3) 0 5) ;; works

(new-if (= 1 1) 0 5) ;; works

;; it's not working
;; it should stop with error that out of memory
(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
         guess
	 (sqrt-iter (improve guess x)
	            x)))


; if 가 왜 특별한 형태 (special form)이여 하는가?
; if 가 특별한 형태일 때만 조건에 맞는 프로시져만 실행된다.
; 위 프로시져에선 new-if 도 프로시져이다. 
; 그리고 Scheme 의 실행기는 정의대로 계산법으로 실행되기 때문에 new-if 로 주어진 3 arguments 가 모두 실행된 뒤에 전달 된다.
; 그러므로 조건과 상관 없이 sqrt-iter 가 재귀로 계속 실행되며 무한재귀 상태에 빠진다.

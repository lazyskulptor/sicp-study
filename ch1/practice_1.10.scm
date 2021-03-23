#lang sicp

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
	((= y 1) 2)
	(else (A (- x 1)
		 (A x (- y 1))))))

; (A 1 10) ;; 1024
; (A 2 4) ;; 65536
; (A 3 3) ;; 65536


;; (define (f n) (A 0 n))
;; (f n) 은 2n 을 의미한다.

;; (define (g n) (A 1 n))
;; (f n) 은 2n 을 의미한다.

;; (define (h n) (A 2 n))

;; if n = 1, (A 2 n) = 2
;; if n > 2
;; (A 2 n) 
;; => (A (- 2 1)
;;   (A 2 (- n 1))
;; => (A 1 (A 2 (n-1)))
;; => (g (h (n-1)))
;; => 2 ** (h (n-1))
;; => (h n) 2 ** 2 ** 2 ** 2 ** 2 ... ** 2
;; (h n) 은 2 를 n 번 연달아 제곱하는 꼴이다. ex) (h 4)는 2**2**2**2
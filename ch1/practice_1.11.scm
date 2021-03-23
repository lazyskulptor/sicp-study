#lang sicp

;; recursive 
(define (op-recur n)
  (cond ((< n 3) n)
    (else (+ (op-recur (- n 1))
	     (* 2 (op-recur (- n 2)))
	     (* 3 (op-recur (- n 3)))
  	  )
    )
  )
)

;; iterative 
(define (op n) 
  (if (< n 3) n
              (op-iter 0 1 2 (- n 2))
  )
)


(define (op-iter a b c cnt)
  (cond 
        ((= cnt 0) c)
        (else (op-iter b c (+ (* 3 a) (* 2 b) c) (- cnt 1)))
  )
)

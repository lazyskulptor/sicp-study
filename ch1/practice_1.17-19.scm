#lang sicp

(define (* a b)
  (if (= b 0)
      0
      (+ a (* a (- b 1)))))

(define (double a)
  (+ a a))

(define (halve a)
  (/ a 2))


;; practice 1.17
(define (fast-multi a b)
  (cond ((= b 0) a)
        ((even? b) (fast-multi (double a) (halve b)))
        (else (fast-multi (+ a a) (- b 1)))))


;; practice 1.18
(define (fast-multi-i a b)
  (fast-multi-iter a b 0))

(define (fast-multi-iter a b sum)
  (cond ((= b 0) sum)
        ((even? b) (fast-multi-iter (double a) (halve b) sum))
        (else (fast-multi-iter a (- b 1) (+ a sum)))))

;; practice 1.19
;; a2 = b1 * q + a1 * (p + q)
;; b2 = b1 * p + a1 * q
;; a3 = (b1 * p + a1 * q) * q + {b1 * q + a1 * (p + q)} * (p + q)
;;    = b1 * (2pq + q^2) + a1 * {p^2 + (2 * p * q) + (2 * q^2)}
;;    = b1 * (2pq + q^2) + a1 * {(p^2 + q^2) + (2 * p * q + q^2)}
;; b3 = (b1 * p + a1 * q) * p + (b1 * q + a1 * (p + q)) * q
;;    = b1 * (p^2 + q^2) + a1 * (2 * p * q + q^2)
;; => p` = p^2 + q^2, q` = 2 * p * q + q^2

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (* p p) (* q q))
                   (+ (* 2 p q) (* q q))
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))
          

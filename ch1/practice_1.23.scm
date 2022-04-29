#lang sicp

;; 1.23

;;  *** 100000000019
;;  *** 100000000057
;; .7399999999999949

;; (search-for-primes 1000000000000)
;;  *** 1000000000039
;;  *** 1000000000061
;;  *** 1000000000063
;; 2.2199999999999998 (* 3.0000000000000204)

;; 29 (* 3.3256880733944953)

;; version 1

(define (time-test f n count start-time)
  (cond ((= count 0)
          (display (- (runtime) start-time))
          (newline))
        (else (time-test f n (- count 1) start-time))))
      
  

(define (smallest-divisor-1 n)
  (find-divisor-1 n 2))

(define (find-divisor-1 n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

;; (time-test smallest-divisor-1 100000000003 1000000 (runtime))
;; .3199999999999932
;; (time-test smallest-divisor-1 100000000019 1000000 (runtime))
;; .3200000000000074
;; (time-test smallest-divisor-1 100000000057 1000000 (runtime))
;; .30999999999998806

;; (time-test smallest-divisor-1 1000000000039 1000000 (runtime))
;; .3100000000000023

;; (time-test smallest-divisor-1 1000000000061 1000000 (runtime))
;; .3100000000000023

;; (time-test smallest-divisor-1 1000000000063 1000000 (runtime))
;; .3199999999999932

;; (time-test smallest-divisor-1 10000000000037 1000000 (runtime))
;; .3100000000000023
;; (time-test smallest-divisor-1 10000000000051 1000000 (runtime))
;; .3299999999999983
;; (time-test smallest-divisor-1 10000000000099 1000000 (runtime))
;; .3200000000000074

;; (time-test smallest-divisor-1 100000000000031 1000000 (runtime))
;; .30999999999998806
;; (time-test smallest-divisor-1 100000000000067 1000000 (runtime))
;; .3100000000000023
;; (time-test smallest-divisor-1 100000000000097 1000000 (runtime))
;; .3200000000000074

;; version 2

(define (smallest-divisor-2 n)
  (find-divisor-2 n 2))

(define (find-divisor-2 n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (next x)
  (if (even? x)
      (+ x 1)
      (+ x 2)))

(define (divides? a b)
  (= (remainder b a) 0))

;; (time-test smallest-divisor-2 100000000000067 1000000 (runtime))
;; .3100000000000023
;; (time-test smallest-divisor-2 100000000000097 1000000 (runtime))
;; .3200000000000074

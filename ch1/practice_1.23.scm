#lang sicp

;; 1.23

;; version 1
(define (ff-prime? n) (fast-prime? n 100))

(define (time-test f n count start-time)
  (cond ((= count 0)
          (display (- (runtime) start-time))
          (newline))
        (else
         (f n)
         (time-test f n (- count 1) start-time))))
      
  

(define (smallest-divisor-1 n)
  (find-divisor-1 n 2))

(define (find-divisor-1 n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor-1 n (+ test-divisor 1)))))


;; (time-test smallest-divisor-1 100000000003 10 (runtime))
;; 2.329999999999984
;; (time-test smallest-divisor-1 100000000019 10 (runtime))
;; 2.330000000000041
;; (time-test smallest-divisor-1 100000000057 1000000 (runtime))
;; 2.339999999999975

;; (time-test smallest-divisor-1 1000000000039 10 (runtime))
;; 7.439999999999998
;; (time-test smallest-divisor-1 1000000000061 10 (runtime))
;; 7.3799999999999955
;; (time-test smallest-divisor-1 1000000000063 10 (runtime))
;; 7.360000000000014

;; (time-test smallest-divisor-1 10000000000037 10 (runtime))
;; 23.409999999999968
;; (time-test smallest-divisor-1 10000000000051 10 (runtime))
;; 23.329999999999984
;; (time-test smallest-divisor-1 10000000000099 10 (runtime))
;; 23.399999999999977

;; (time-test smallest-divisor-1 100000000000031 10 (runtime))
;; 73.57999999999998
;; (time-test smallest-divisor-1 100000000000067 10 (runtime))
;; 73.78000000000009
;; (time-test smallest-divisor-1 100000000000097 10 (runtime))
;; 73.85000000000002

;; version 2

(define (smallest-divisor-2 n)
  (find-divisor-2 n 2))

(define (find-divisor-2 n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor-2 n (next test-divisor)))))

(define (next x)
  (if (= x 2)
      3
      (+ x 2)))

(define (divides? a b)
  (= (remainder b a) 0))

;; (time-test smallest-divisor-2 100000000003 10 (runtime))
;; 1.490000000000009   ;; 1.5637583892617248 times
;; (time-test smallest-divisor-1 100000000019 10 (runtime))
;; 1.4399999999999977  ;; 1.6180555555555864 times
;; (time-test smallest-divisor-1 100000000057 1000000 (runtime))
;; 1.4399999999999977  ;; 1.6249999999999851 times

;; (time-test smallest-divisor-2 1000000000039 10 (runtime))
;; 4.600000000000023  ;; 1.6173913043478176 times
;; (time-test smallest-divisor-2 1000000000061 10 (runtime))
;; 4.589999999999975  ;; 1.6078431372549098 times
;; (time-test smallest-divisor-2 1000000000063 10 (runtime))
;; 4.589999999999975  ;; 1.6034858387799682 times

;; (time-test smallest-divisor-2 10000000000037 10 (runtime))
;; 14.32000000000005  ;; 1.6347765363128413
;; (time-test smallest-divisor-2 10000000000051 10 (runtime))
;; 14.410000000000025  ;; 1.6190145732130425 times
;; (time-test smallest-divisor-2 10000000000099 10 (runtime))
;; 14.480000000000018   ;; 1.6160220994475103 times

;; (time-test smallest-divisor-2 100000000000031 10 (runtime))
;; 45.329999999999984 ;; 1.6232075887932937 times
;; (time-test smallest-divisor-2 100000000000067 10 (runtime))
;; 45.239999999999895 ;; 1.630857648099033 times
;; (time-test smallest-divisor-2 100000000000097 10 (runtime))
;; 45.30000000000007 ;; 1.630242825607062 times

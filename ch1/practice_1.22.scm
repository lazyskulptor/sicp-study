;; 1.22
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes r)
  (if (even? r)
      (worker (+ r 1) 3 (runtime))
      (worker r 3 (runtime))))

(define (worker r counter start-time)
  (cond ((= counter 0)
         (display (- (runtime) start-time))
         (newline))
        ((prime? r)
         (display " *** ")
         (display r)
         (newline)
         (worker (+ r 2) (- counter 1) start-time))
        (else
         (worker (+ r 2) counter start-time))))

;;  √10 = 3.162
;; (search-for-primes 100000000000)
;;  *** 100000000003
;;  *** 100000000019
;;  *** 100000000057
;; .7399999999999949

;; (search-for-primes 1000000000000)
;;  *** 1000000000039
;;  *** 1000000000061
;;  *** 1000000000063
;; 2.2199999999999998 (* 3.0000000000000204)

;; (search-for-primes 10000000000000)
;;  *** 10000000000037
;;  *** 10000000000051
;;  *** 10000000000099
;; 8.72    (* 3.9279279279279287)

;; (search-for-primes 100000000000000)
;;  *** 100000000000031
;;  *** 100000000000067
;;  *** 100000000000097
;; 29 (* 3.3256880733944953)

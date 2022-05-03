#lang sicp


;; 561
(fast-prime? 561 100)
;Value: #t

(prime? 561)
;Value: #f

(define (fermat-test-all-nums n)
  (fermat-test-iter n 2))

(define (fermat-test-iter n count)
  (cond ((< count 2) #f)
        ((= count n) #t)
        ((expmod count n n)
         (carmichael-test-iter n (+ 1 count)))
        (else #f)))

  
(define (carmichael-test n)
  (and (fermat-test-all-nums n) (prime? n)))

561, 1105, 1729, 2465, 2821, 6601

(carmichael-test 561)
;Value: #f

(carmichael-test 1105)
;Value: #f

(carmichael-test 1729)
;Value: #f

(carmichael-test 2465)
;Value: #f

(carmichael-test 2821)
;Value: #f

(carmichael-test 6601)
;Value: #f

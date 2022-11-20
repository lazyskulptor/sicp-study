;; Exercise 3.39-42

;; 3.39
(define x 10)

(define s (make-serializer))

(parallel-execute (lambda () (set! x ((s (lambda () (* x x)))))) ;; Let me call A
                  (s (lambda () (set! x (+ x 1))))) ;; Let me call B

;; CASE: A READ, A SAVE, B TX. => 11
;; CASE: A READ, B TX, A SAVE => 100
;; CASE: B TX, A READ, A SAVE => 121

;; 3.40
(define x 10)

(parallel-execute (lambda () (set! x (* x x)))    ;; A
                  (lambda () (set! x (* x x x)))) ;; B
;; CASE: A READ, A SAVE, B READ, B SAVE => 1000000
;; CASE: A READ, B READ, A SAVE, B SAVE => 1000
;; CASE: A READ, A READ, B SAVE, A SAVE => 100

;; ... Other resuls are same with above

(define s (make-serializer))
                  
(parallel-execute (s (lambda () (set! x (* x x))))   ;; A
                  (s (lambda () (set! x (* x x x))))) ;; B
;; only 1000000 case remains


;; 3.41
;; I don't agree
;; Becase return a value is single action, No other procedure can iterleave.

;;3.42
;; I think procedure make hook on original procedure. So it's results are same.

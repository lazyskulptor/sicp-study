;; Exercise 3.5 - 3.6

;; 3.5
;;GIVEN
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

;; SOLUTION
(define (estimate-integral P x1 x2 y1 y2 trials)
  (* (* (- x2 x1) (- y2 y1))
     (monte-carlo trials
                  (lambda ()
                    (P (random-in-range x1 x2)
                       (random-in-range y1 y2))))))

;; TEST
(define (ex-circle x y)
  (let ((left (+ (expt (- x 5) 2) (expt (- y 7) 2)))
        (right (expt 3 2)))
    (or (= left right) (< left right))))

(define (estimate-pi2 trials)
  (/ (estimate-integral ex-circle 2.0 8.0 4.0 10.0 trials) 9.0))

;; RESULT

(estimate-pi2 10)
;Value: 2.8

(estimate-pi2 100)
;Value: 3.48

(estimate-pi2 1000)
;Value: 3.156

(estimate-pi2 10000)
;Value: 3.1324

(estimate-pi2 100000)
;Value: 3.14428

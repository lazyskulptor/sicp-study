;; sicp practice 1.35 - 

;; 1.35
(define (golden-ratio)
  (fixed-point (lambda (y) (+ 1 (/ 1 y)))
               1.0))
  
;; 1.36
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display next)
      (newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (x^x)
  (fixed-point (lambda (x) (/ (log 1000) (log x)))
               2.0))
  

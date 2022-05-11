;; sicp 2.1

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cond ((= d 0) (error "Denominator can't be zeor"))
          ((< d 0) (cons (/ (* -1 n) g) (/ (* -1 d) g)))
          (else (cons (/ n g) (/ d g))))))

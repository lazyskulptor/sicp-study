;; sicp 2.4 - 2.6

;; 2.4

(define (cons4 x y)
  (lambda (m) (m x y)))

(define (car4 z)
  (z (lambda (p q) p)))

(define (cdr4 z)
  (z (lambda (p q) q)))


(car (cons 1 2))
((lambda (m) (m 1 2)) (lambda (p q) p))
((lambda (p q) p) 1 2)
;Value: 1

(cdr (cons 1 2))
((lambda (m) (m 1 2)) (lambda (p q) q))
((lambda (p q) q) 1 2)
;Value: 2

(car (cons4 "a" "b"))
;Value: "a"
(cdr (cons4 "a" "b"))
;Value: "b"


;; 2.5
(define (cons x y)
  (* (expt 2 x) (expt 3 y)))

(define (car z)
  (define (divide n i)
    (if (= (remainder n 2) 0)
        (divide (/ n 2) (+ i 1))
        i))
  (divide z 0))

(define (cdr z)
  (define (divide n i)
    (if (= (remainder n 3) 0)
        (divide (/ n 3) (+ i 1))
        i))
  (divide z 0))


;; 2.6

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))


(add-1 zero)
(lambda (f) (lambda (x) (f ((zero f) x))))
(lambda (f) (lambda (x) (f (((lambda (g) (lambda (y) y)) f) x))))
(lambda (f) (lambda (x) (f ((lambda (y) y) x))))
(lambda (f) (lambda (x) (f x)))

(define one (lambda (f) (lambda (x) (f x))))

(add-1 one)
(lambda (f) (lambda (x) (f (((lambda (g) (lambda (y) (g (g y)))) f) x))))
(lambda (f) (lambda (x) (f ((lambda (y) (f (f y))) x))))
(lambda (f) (lambda (x) (f (f (f x)))))

(define two (lambda (f) (lambda (x) (f (f x)))))

(define (add m n)
  (lambda (f) (lambda (x) ((m f) ((n f) x)))))


((zero (lambda (x) (+ 1 x))) 0)
;Value: 0

((one (lambda (x) (+ 1 x))) 0)
;Value: 1

((two (lambda (x) (+ 1 x))) 0)
;Value: 2

(((add one two) (lambda (x) (+ 1 x))) 0)
;Value: 3

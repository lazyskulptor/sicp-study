;; sicp practice 1.40 - 1.46

;; 1.40

(define (cubic a b c)
  (lambda (x) (+ (* x x x) (* a x x) (* b x) c)) )
(newtons-method (cubic a b c) 1)


;; 1.41

(define (inc x) (+ x 1))
(define (double f)
  (lambda (x) (f (f x))))


(((double (double double)) inc) 5)
;Value: 21

;; (((double (lambda (x) (double (double x)))) inc) 5)

(((lambda (b) ((lambda (a) (double (double a))) ((lambda (a) (double (double a))) b))) inc) 5)

(((lambda (a) (double (double a))) ((lambda (a) (double (double a))) inc)) 5)

(((lambda (a) (double (double a))) ((lambda (a) (double (double a))) inc)) 5)

(((lambda (a) (double (double a))) (double (double inc))) 5)

(((lambda (a) (double (double a))) (double (lambda (x) (inc (inc x))))) 5)

;; inc 가 총 16 번 적용됨

;; 1.42 

(define (compose f g)
  (lambda (x) (f (g x))))

((compose square inc) 6)

;; 1.43

(define (repeated f n)
  (if (= n 1)
      (lambda (x) (f x))
      (lambda (x) ((repeated f (- n 1)) (f x)))))

(define (repeated-iter f n)
  (define (iter rs cnt)
    (if (= cnt n)
        (f rs)
        (iter (f rs) (+ cnt 1))))
  (lambda (x)
    (iter x 1)))


;; 1.44
(define dx 0.00001)

(define (smooth f)
  (lambda (x) (/ (+ (f x) (f (+ x dx)) (f (- x dx))) 3)))

(define (smooth-nth f n)
  ((repeated-iter smooth n) f))

;; 1.45
(define (nth-sqrt x n)
  (fixed-point ((repeated average-damp (+ n))
                (lambda (y) (/ x (expt y (- n 1)))))
               1.0))


;; 1.46

;; sqrt
(define (average x y)
  (/ (+ x y) 2))

(define (iterative-improve-1 improve good-enough?)
  (define (iter guess)
    (if (good-enough? guess)
        guess
        (iter (improve guess))))
  (lambda (guess) (iter guess)))

(define (sqrt x)
  (define (improve guess)
    (average guess (/ x guess)))
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  ((iterative-improve-1 improve good-enough?) 1.0))

;; fixed-point
(define (iterative-improve-2 improve good-enough?)
  (define (iter guess)
    (let ((next (improve guess)))
      (if (good-enough? guess next)
                    next
                    (iter next))))
  (lambda (guess) (iter guess)))

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  ((iterative-improve-2 f close-enough?) first-guess))

(define (sqrt-fixed x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

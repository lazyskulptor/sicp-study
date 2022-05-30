;; Exercise 2.77-80

;; 2.77
(define (install-complex-selector-package)
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  'done)

(magnitude z)
(apply-generic '(complex) z) ;; 1
(apply-generic '(polar) z) ;; 2
(car z)

(magnitude z)
(apply-generic '(complex) z) ;; 1
(apply-generic '(rectangular) z) ;; 2
(sqrt (+ (square (real-part z))
             (sqrt (imag-part z))))

;; (put 'real-part '(complex) real-part) 실행함으로 (apply-generic 'magnitude z) 로 이어준다.


;; 2.78
(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (attach-tag type-tag contents)
  (if (number? contents)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'scheme-number)
        (else
         (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (else
         (error "Bad tagged datum -- CONTENTS" datum))))


;; 2.79
(define (install-generic-equ?)
  (define (s-r-equ? s r)
    (= (* s (denom r)) (numer r)))
  (define (s-c-equ? s c)
    (and (= s (real-part c)) (= 0 (imag-part c))))

  (define (r-s-equ? r s)
    (s-r-equ? s r))
  (define (r-r-equ? r1 r2)
    (= (* (denom r1) (numer r2)) (* (numer r1) (denom r2))))
  (define (r-c-equ? r c)
    (and (s-r-equ? (real-part c) r) (= 0 (imag-part c))))

  (define (c-s-equ? c s)
    (s-c-equ? s c))
  (define (c-r-equ? c r)
    (r-c-equ? r c))
  (define (c-c-equ? c1 c2)
    (and (= (real-part c1) (real-part c2)) (= (imag-part c1) (imag-part c2))))
    
  (put 'equ? '(scheme-number scheme-number) =)
  (put 'equ? '(scheme-number rational) s-r-equ?)
  (put 'equ? '(scheme-number complex) s-c-equ?)
  (put 'equ? '(rational scheme-number) r-s-equ?)
  (put 'equ? '(rational rational) r-r-equ?)
  (put 'equ? '(rational complex) r-c-equ?)
  (put 'equ? '(complex scheme-number) c-s-equ?)
  (put 'equ? '(complex rational) c-r-equ?)
  (put 'equ? '(complex complex) c-c-equ?)
  'done)

;; 2.80

(define (install-generic-zero?)
  (define (s-zero? n) (= 0 n))
  (define (r-zero? n) (= 0 (denom n)))
  (define (c-zero? n) (and (= 0 (real-part n)) (= 0 (imag-part n))))

  (put '=zero? '(scheme-number) s-zero?)
  (put '=zero? '(rational) r-zero?)
  (put '=zero? '(complex) c-zero?))

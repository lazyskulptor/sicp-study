;; Exercise 2.73-74

;; 2.73
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        (else
         (error "unknown expression type -- DERIV" exp))))

;; ==>
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp)
               var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

;; a sum?, product? 분기가 data-directed 로 꺼내졌다.
;; numbers?, same-variable의 경우 함께 묶여서 처리될 수 없는 이유는
;; dispatch 할 때 exp가 list이며 첫번째 값의 따라 작동하나
;; sum?, product?는 exp값 자체를 검사함으로 함께 묶여서 처리될 수 없다.

;; b
(define (make-sum a1 a2) (list '+ a1 a2))
(define (addend s) (cadr s))
(define (augend s) (caddr s))

(define (make-product m1 m2) (list '* m1 m2))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

(define (install-sum-package)
  (deriv (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
  (put 'deriv '+ deriv)
  'done)

(define (install-product-package)
  (deriv (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
  (put 'deriv '* deriv)
  'done)

;; c
(define (make-exponentiation b n) (list '** b n))
(define (base x) (cadr x))
(define (exponent x) (caddr x))

(define (install-exp-package)
  (deriv (make-product
          (make-product (exponent exp)
                        (make-exponentiation (base exp) (make-sum (exponent exp) -1)))
          (deriv (base exp) var)))
  (put 'deriv '** deriv)
  'done)

;; d
((get (operator exp) 'deriv) (operands exp) var)

(define (install-sum-package)
  (deriv (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
  (put '+ 'deriv deriv)
  'done)



;; 2.74

;; 2.75

(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          ((eq? op 'real-part) (* (magnitude z) (sin (angle z))))
          ((eq? op 'imag-part) (* (magnitude z) (cos (angle z))))
          (else
           (error "Unknown op -- MAKE-FROM-MAG-ANG" op))))
  dispatch)


;; 2.76
;; generic operations with explicit dispatch
;; data-directed style
;; message-passing-style

;; Type a, b c
;; Operation op1, op2, op3

;; Explicit style
(define (op1-a x) )
(define (op2-a x) )
(define (op3-a x) )

(define (op1-b x) )
(define (op2-b x) )
(define (op3-b x) )

(define (op1-c x) )
(define (op2-c x) )
(define (op3-c x) )

(define (op1 x)
  (cond ((eq? (type-tag x) 'a) (op1-a (contents x)))
        ((eq? (type-tag x) 'b) (op1-b (contents x)))
        (else (op1-c x))))

(define (op2 x)
  (cond ((eq? (type-tag x) 'a) (op2-a (contents x)))
        ((eq? (type-tag x) 'b) (op2-b (contents x)))
        (else (op2-c (contents x)))))

(define (op3 x)
  (cond ((eq? (type-tag x) 'a) (op3-a (contents x)))
        ((eq? (type-tag x) 'b) (op3-b (contents x)))
        (else (op3-c (contents x)))))

;; Data-directed
(define (install-a)
  (define (op1 x) x)
  (define (op2 x) x)
  (define (op3 x) x)
  (put 'op1 'a op1)
  (put 'op2 'a op2)
  (put 'op3 'a op3)
  'done)
  
(define (install-b)
  (define (op1 x) x)
  (define (op2 x) x)
  (define (op3 x) x)
  (put 'op1 'b op1)
  (put 'op2 'b op2)
  (put 'op3 'b op3)
  'done)

(define (install-c)
  (define (op1 x) x)
  (define (op2 x) x)
  (define (op3 x) x)
  (put 'op1 'c op1)
  (put 'op2 'c op2)
  (put 'op3 'c op3)
  'done)

(define (op1 x) (apply (get 'op1 (type-tag x)) (contents x)))
(define (op2 x) (apply (get 'op2 (type-tag x)) (contents x)))
(define (op3 x) (apply (get 'op3 (type-tag x)) (contents x)))

;; Message-passing style
(define (apply-generic op arg) (arg op))

(define (make-a x)
  (define (dispatch op)
    (cond ((eq? op 'op1) x)
          ((eq? op 'op2) x)
          ((eq? op 'op3) x)
          (else
           (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)

(define (make-b x)
  (define (dispatch op)
    (cond ((eq? op 'op1) x)
          ((eq? op 'op2) x)
          ((eq? op 'op3) x)
          (else
           (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)

(define (make-c x)
  (define (dispatch op)
    (cond ((eq? op 'op1) x)
          ((eq? op 'op2) x)
          ((eq? op 'op3) x)
          (else
           (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)

(define (op1 x) (apply-generic 'op1 x))
(define (op2 x) (apply-generic 'op2 x))
(define (op3 x) (apply-generic 'op3 x))

;; Data-directed, message-passing 스타일 둘 다 새 타입이 추가되면 install이나 constructor를 새로 만들면 된다.
;; 반면 Operator가 추가 되야 하는 경우는 data-directed의 경우는 'install'을 추가로 만들면 되나
;; message-passing의 경우 기존 constructor를 변경해야 한다.

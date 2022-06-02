;; sicp Exercise 2.81 - 2.86

;; 2.81
(define (scheme-number->scheme-number n) n)
(define (complex->complex z) z)
(put-coercion 'scheme-number 'scheme-number
              scheme-number->scheme-number)
(put-coercion 'complex 'complex complex->complex)

;; a
(define (exp x y) (apply-generic 'exp x y))

(put 'exp '(scheme-number scheme-number)
     (lambda (x y) (tag (expt x y))))

;; '(complex complex) 타입의 프로시져가 없고 complex의 super type이 없기 때문에
;; 계속해서 complex->complex가 작동하며 무한반복에 빠진다.

;; b
;; 같은 방법으로 작동한다.
;; 다만 같은 타입일 때 예외 적용을 하면 상위 타입까지 가서 멈추는 불필요한 작동을 하지 않는다.

;; c
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (and (= (length args) 2) (not (equal? (car type-tags) (cadr type-tags))))
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond (t1->t2
                         (apply-generic op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic op a1 (t2->t1 a2)))
                        (else
                         (error "No method for these types"
                                (list op type-tags))))))
              (error "No method for these types"
                     (list op type-tags)))))))
                        

;; 2.82
(define (apply-generic op . args)
  (define (coerce target args rs)
    (if (null? args)
        rs
        (let ((t->target (get-coercion target (car args))))
          (coerce target (cdr args) (append rs (t->target (car args)))))))
  (define (try-it pre post)
    (let ((args (append pre post)))
      (cond ((null? post) nil)
            ((= 1 (length args)) (apply apply-generic (append (list op) args)))
            ((apply apply-generic (append (list op) pre
                                          (append (list (car post) (coerce (car post) (cdr post) '()))))))
            (try-it (append pre (car post)) (cdr post)))))
    
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (try-it (car args) (cdr args))
              (error "No method for these types" (list op type-tags)))))))

;; 만약, 함수가 '(complex rational scheme-number complex) 형태라고 한다면 

;; 2.83
(put-coercion 'scheme-number 'rational
              (get 'raise 'scheme-number))

(put-coercion 'rational 'complex
              (get 'raise 'rational))

(define (install-raise)
  (define (s->r s) (make-rational s 1))
  (define (r->z r) (make-complex-from-real-imag r 0))

  (put 'raise '(scheme-number) s->r)
  (put 'raise '(rational) r->z))

(define (raise x) (apply-generic 'raise x))


;; 2.84

;; 2.85
;; 2.86

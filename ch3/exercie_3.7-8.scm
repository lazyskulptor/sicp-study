;; Exercise 3.7 - 3.8

;; 3.7
;; GIVEN from exercise 3.3
(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m pw)
    (cond ((not (eq? pw password)) (lambda (_) "Incorret password"))
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknow request --- MAKE-ACCOUNT"
                       m))))
  dispatch)

;; SOLUTION
(define (make-joint account password new-password)
  (lambda (m pw)
    (cond ((not (eq? pw new-password))
           (lambda (_) "Incorret password"))
          ((eq? m 'withdraw)
           (account 'withdraw password))
          ((eq? m 'deposit)
           (account 'deposit password))
          (else (account 'else password)))))
  

;; TEST
(define hj (make-account 10000 '1234))
(define friend (make-joint hj '1234 'abcd))


;; 3.8

;; SOLUTION
(define rs 1)
(define (f a)
  (if (= rs 0)
      0
      (begin (set! rs a) rs)))

;; TEST

(f 1)
;Value: 1

(f 0)
;Value: 0

(f 1)
;Value: 0

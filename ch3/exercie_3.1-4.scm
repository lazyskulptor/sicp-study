;; Exercise 3.1 - 3.4

;; 3.1
(define (make-accumulator sum)
  (lambda (amount)
    (set! sum (+ sum amount))
    sum))

;; 3.2
(define (make-monitored f)
  (let ((count 0))
    (define (how-many-calls?) count)
    (define (reset-count)
      (set! count 0) 0)
    (define (wrapper . args)
      (set! count (+ count 1))
      (apply f args))
    (define (dispatch . m)
      (cond ((eq? (car m) 'how-many-calls?) (how-many-calls?))
            ((eq? (car m) 'reset-count) (reset-count))
            (else (apply wrapper m))))
    dispatch))

;; 3.3
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

(define acc (make-account 100 'secret-password))

;; 3.4
(define (make-account balance password)
  (let ((consecutive 0))
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (dispatch m pw)
      (cond ((not (eq? pw password))
             (set! consecutive (+ consecutive 1))
             (if (> consecutive 7)
                 (lambda (_) "Calling the cops")
                 (lambda (_) "Incorret password")))
            ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            (else (error "Unknow request --- MAKE-ACCOUNT"
                        m))))
    dispatch))
  

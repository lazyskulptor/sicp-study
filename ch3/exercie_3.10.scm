;; Exercise 3.10

;; Case 1
(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))

(define W1 (make-withdraw 100))
;; (                         Gloabl Env                      )
;;                    ^                                     ^
;;                    |                                     |
;;              (balance: 100)  (param: balance, body: ...)( )
;;                    ^  
;;   ( )( )___________|  
;;    |
;;    v
;;   param: amount
;;   body: (if ...)

(W1 50)
;; (                         Gloabl Env                      )
;;                    ^                                     ^
;;                    |                                     |
;;              (balance: 100)  (param: balance, body: ...)( )
;;                    ^  ^
;;   ( )( )___________|  |
;;    |                  |
;;    v                  (amount: 50)
;;   param: amount
;;   body: (if ...)

;; After procedured
;; (                         Gloabl Env                      )
;;                    ^                                     ^
;;                    |                                     |
;;              (balance:  50)  (param: balance, body: ...)( )
;;                    ^  
;;   ( )( )___________| 
;;    |                  
;;    v                  
;;   param: amount
;;   body: (if ...)


(define W2 (make-withdraw 100))
;; (                         Gloabl Env                      )
;;                    ^                              ^
;;                    |                              |
;;              (balance:  50)                 (balance:  100)
;;   ( )( )-----------^             ( )( )-----------^
;;    |______________________________|
;;    v                             
;;   param: amount                   
;;   body: (if ...)                 



;; Case 2
(define (make-withdraw initial-amount)
  (let ((balance initial-amount))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))


(define W1 (make-withdraw 100))
;; (                         Gloabl Env                      )
;;                    ^                          |          ^
;;                    |                          v          |
;;              (initial-amount: 100)  (param: initial-amount, body: ...)( )
;;                    ^  
;;                    |  
;;              (let ((balance initial-amount)) ...)


;; (                         Gloabl Env                      )
;;                    ^                          |          ^
;;                    |                          v          |
;;              (balance: 100)          (param: initial-amount, body: ...)( )
;;                    ^
;;   ( )( )___________|
;;    |
;;    v
;;   param: amount
;;   body: (if ...)


;; .
;; .
;; .

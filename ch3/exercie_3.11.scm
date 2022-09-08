;; Exercise 3.11

;; GIVEN
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unkown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)


;; INIT
;; (    (make-account)       |Gloabl Env|                      )
;;            ^
;;   ( )( )___|
;;    |        
;;    v             
;;   param:  balance 
;;   body: (define (with ...) 
;;         (define (depo ...) 
;;         (define (disp ...) 

;; WHEN
(define acc (make-account 50))
;; (    (make-account),       |Gloabl Env|                                acc)
;;            ^          E1->(  balance: 50, withdraw, deposit, dispatch)  ^
;;   ( )( )___|                               ^         ^          ^  ^    |
;;    |                                       |         |          |  |    |
;;    v                                       |         |          |   ----+--|
;;   param:  balance                      ( )( )    ( )( )     ( )( )      |  |
;;   body: (define (with ...)<-------------|         |          |          |  |
;;         (define (depo ...)<-----------------------|          |          |  |
;;         (define (disp ...)<----------------------------------|---------( )( )

;; WHEN
((acc 'deposit) 40)
;; (    (make-account),       |Gloabl Env|                                acc)
;;            ^          E1->(  balance: 50, withdraw, deposit, dispatch)  ^
;;   ( )( )___|                ^        ^     ^         ^          ^  ^    |
;;    |                        |        |     |         |          |  |    |
;;    v                        |        |     |         |          |   ----+--|
;;   param:  balance           |        | ( )( )    ( )( )     ( )( )      |  |
;;   body: (define (with ...)<-+--------+--|         |          |          |  |
;;         (define (depo ...)<-+--------+------------|          |          |  |
;;         (define (disp ...)<-+--------+-----------------------|---------( )( )
;;                             |        |
;;             E2->( m: 'deposit)  E3->( amount: 40)
;;                    

;; WHEN
((acc 'withdraw) 60)
;; (    (make-account),       |Gloabl Env|                                acc)
;;            ^          E1->(  balance: 90, withdraw, deposit, dispatch)  ^
;;   ( )( )___|                ^        ^     ^         ^          ^  ^    |
;;    |                        |        |     |         |          |  |    |
;;    v                        |        |     |         |          |   ----+--|
;;   param:  balance           |        | ( )( )    ( )( )     ( )( )      |  |
;;   body: (define (with ...)<-+--------+--|         |          |          |  |
;;         (define (depo ...)<-+--------+------------|          |          |  |
;;         (define (disp ...)<-+--------+-----------------------|---------( )( )
;;                             |        |
;;             E2->( m: 'withdraw)  E3->( amount: 60)


;; AFTER EVALUATION
;; (    (make-account),       |Gloabl Env|                                acc)
;;            ^          E1->(  balance: 30, withdraw, deposit, dispatch)  ^
;;   ( )( )___|                ^        ^     ^         ^          ^  ^    |
;;    |                        |        |     |         |          |  |    |
;;    v                        |        |     |         |          |   ----+--|
;;   param:  balance           |        | ( )( )    ( )( )     ( )( )      |  |
;;   body: (define (with ...)<-------------|         |          |          |  |
;;         (define (depo ...)<-----------------------|          |          |  |
;;         (define (disp ...)<----------------------------------|---------( )( )


(define acc2 (make-account 100))
;; (    (make-account),       |Gloabl Env|                                acc        acc2)
;;            ^          E1->(  balance: 30, withdraw, deposit, dispatch)  ^          ^   E2->(  balance: 100, withdraw, deposit, dispatch)
;;   ( )( )___|                ^        ^     ^         ^          ^  ^    |          |          ^         ....
;;    |                        |        |     |         |          |  |    |          |  |-------|
;;    v                        |        |     |         |          |   ----+--|       |  |
;;   param:  balance           |        | ( )( )    ( )( )     ( )( )      |  |       |  |
;;   body: (define (with ...)<-------------|         |          |          |  |       |  |
;;         (define (depo ...)<-----------------------|          |          |  |       |  |
;;         (define (disp ...)<----------------------------------|---------( )( )     ( )( )
;;                                                              |---------------------| 

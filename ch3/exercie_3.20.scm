;; Exercise 3.20
;; GIVEN
(define (cons x y)
  (define (set-x! v) (set! x v))
  (define (set-y! v) (set! y v))
  (define (dispatch m)
    (cond ((eq? m 'car) x)
          ((eq? m 'cdr) y)
          ((eq? m 'set-car!) set-x!)
          ((eq? m 'set-cdr!) set-y!)
          (else (error "Undefined operation -- CONS" m))))
  dispatch)

(define (car z) (z 'car))
(define (cdr z) (z 'cdr))

(define (set-car! z new-value)
  ((z 'set-car!) new-value)
  z)
(define (set-cdr! z new-value)
  ((z 'set-cdr!) new-value)
  z)


;; INIT

;;                 .....................................
;;                    ^     ^     ^     ^          ^
;;                    |     |     |     |          |
;;                 (*)() (*)() (*)() (*)()      (*)()
;;                  ^     ^     ^     ^          ^
;;                  |     |     |     |          |
;; [Gloabl Env|  (cons )(car )(cdr )(set-car! )(set-cdr! )                             ]

(define x (cons 1 2))

;;                 .....................................
;;                    ^     ^     ^     ^          ^
;;                    |     |     |     |          |
;;                 (*)() (*)() (*)() (*)()      (*)()
;;                  ^     ^     ^     ^          ^
;;                  |     |     |     |          |
;; [Gloabl Env|  (cons )(car )(cdr )(set-car! )(set-cdr! )                             ]
;; [    x                                                                              ]
;;      |
;;      v
;;     [*][*]-> [E1| x: 1, y: 2, set-x!, set-y! ]
;;      |
;;      v
;;     param: m
;;     body: (cond ((eq? m 'car)...
;;                 ((eq? m 'cdr)...
;;                 ((eq? m 'set-car!)...
;;                 ((eq? m 'set-cdr!)...
;;                 ...


(define x (cons 1 2))

;;                 .....................................
;;                    ^     ^     ^     ^          ^
;;                    |     |     |     |          |
;;                 (*)() (*)() (*)() (*)()      (*)()
;;                  ^     ^     ^     ^          ^
;;                  |     |     |     |          |
;; [Gloabl Env|  (cons )(car )(cdr )(set-car! )(set-cdr! )                             ]
;; [    z                                --->        x                                 ]
;;      |               ________________/            |                                
;;      v               |     |                      v
;;     [*][*]-> [E2| x: *, y: *, set-x!, set-y! ]              [*][*]-> [E1| x: 1, y: 2 ]
;;      |                                            |
;;      v                                            v
;;     param: m                                     param: m
;;     body: (cond ((eq? m 'car)...                 body: (cond ((eq? m 'car)...
;;                 ((eq? m 'cdr)...                             ((eq? m 'cdr)...
;;                 ((eq? m 'set-car!)...                        ((eq? m 'set-car!)...
;;                 ((eq? m 'set-cdr!)...                        ((eq? m 'set-cdr!)...
;;                 ...                                          ...


(set-car! (cdr z) 17)
;; (cdr z)
;;   => (z 'cdr)
;;   => y of E2
;;   => Pointer to x in Global Env
;;   => (set-car! x 17)

;; (set-cdr! x 17)
;;   => ((x 'set-car!) 17)
;;   => ((x 'set-car!) 17)
;;   => (set-x! 17) ;; In E1

;;                 .....................................
;;                    ^     ^     ^     ^          ^
;;                    |     |     |     |          |
;;                 (*)() (*)() (*)() (*)()      (*)()
;;                  ^     ^     ^     ^          ^
;;                  |     |     |     |          |
;; [Gloabl Env|  (cons )(car )(cdr )(set-car! )(set-cdr! )                             ]
;; [    z                                --->        x                                 ]
;;      |               ________________/            |                                
;;      v               |     |                      v
;;     [*][*]-> [E2| x: *, y: *, set-x!, set-y! ]              [*][*]-> [E1| x: 17, y: 2 ]
;;      |                                            |
;;      v                                            v
;;     param: m                                     param: m
;;     body: (cond ((eq? m 'car)...                 body: (cond ((eq? m 'car)...
;;                 ((eq? m 'cdr)...                             ((eq? m 'cdr)...
;;                 ((eq? m 'set-car!)...                        ((eq? m 'set-car!)...
;;                 ((eq? m 'set-cdr!)...                        ((eq? m 'set-cdr!)...
;;                 ...                                          ...

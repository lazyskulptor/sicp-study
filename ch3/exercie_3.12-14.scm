;; Exercise 3.12

;; GIVEN
(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))
(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

;; WHEN 1
(define x (list 'a 'b))
;; x-> [*][*]->[*][/]
;;      |       |
;;      v       v
;;     [a]     [b]

;;
(define y (list 'c 'd))
;; y-> [*][*]->[*][/]
;;      |       |
;;      v       v
;;     [c]     [d]

;; 
(define z (append x y))
;; x-> [*][*]->[*][/]
;;      |       |
;;      v       v
;;     [a]     [b]
;;
;; y-> [*][*]->[*][/]
;;      |       |
;;      v       v
;;     [c]     [d]
;;
;; z ->[*][*]->[*][*]->[*][*]->[*][/]
;;      |       |       |       |
;;      v       v       v       v
;;     [a]     [b]     [c]     [d]

z
;; (a b c d)

(cdr x)
;; (b)

(define w (append! x y))

w
;; (a b c d)

(cdr x)
;; (b c d)



;; Exercise 3.13
;; GIVEN
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

;; WHEN
(define z (make-cycle (list 'a 'b 'c)))
;;       __________________________
;;      |                          |   
;;      v                          |   
;; z ->[*][*]->[*][*]->[*][*]->[*][*]
;;      |       |       |       |
;;      v       v       v       v
;;     [a]     [b]     [c]     [d]


;; Exercise 3.14
;; GIVEN
(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))


;; WHEN
(define v (list 'a 'b 'c 'd))
;; v ->[*][*]->[*][*]->[*][*]->[*][/]
;;      |       |       |       |
;;      v       v       v       v
;;     [a]     [b]     [c]     [d]


;; WHEN
(define w (mystery v))

;; loop 1 START
;;      x      y->[/]
;;      |       
;;      v       
;; v ->[*][*]->[*][*]->[*][*]->[*][/]
;;      |       |       |       |
;;      v       v       v       v
;;     [a]     [b]     [c]     [d]

;; loop 1 END
;;      x      temp
;;      |       |
;;      v       v
;; v ->[*][/]  [*][*]->[*][*]->[*][/]
;;      |       |       |       |
;;      v       v       v       v
;;     [a]     [b]     [c]     [d]

;; loop 2 START
;;      y       x
;;      |       |
;;      v       v
;; v ->[*][/]  [*][*]->[*][*]->[*][/]
;;      |       |       |       |
;;      v       v       v       v
;;     [a]     [b]     [c]     [d]

;; loop 2 END
;;       __________    temp
;;      |       x  |    |
;;      |       |  |    |
;;      v       v  |    v
;; v ->[*][/]  [*][*]  [*][*]->[*][/]
;;      |       |       |       |
;;      v       v       v       v
;;     [a]     [b]     [c]     [d]


;; loop 3 START
;;       __________     x
;;      |       y  |    |
;;      |       |  |    |
;;      v       v  |    v
;; v ->[*][/]  [*][*]  [*][*]->[*][/]
;;      |       |       |       |
;;      v       v       v       v
;;     [a]     [b]     [c]     [d]

;; loop 3 END
;;       __________     x      temp 
;;      |          |    |       |
;;      |          |    |       |
;;      v          |    v       v
;; v ->[*][/]  [*][*]  [*][*]->[*][/]
;;      |       ^\      |  |    |
;;      v       | v     v  |    v
;;     [a]      |[b]   [c] |   [d]
;;              |__________|

;; loop 4 START
;;       __________     y       x
;;      |          |    |       |
;;      |          |    |       |
;;      v          |    v       v
;; v ->[*][/]  [*][*]  [*][*]->[*][/]
;;      |       ^\      |  |    |
;;      v       | v     v  |    v
;;     [a]      |[b]   [c] |   [d]
;;              |__________|

;; loop 4 END
;;       __________      __________    temp->[/]
;;      |          |    |       x  |
;;      |          |    |       |  |
;;      v          |    v       v  |
;; v ->[*][/]  [*][*]  [*][*]->[*][*]
;;      |       ^\      |  |    |
;;      v       | v     v  |    v
;;     [a]      |[b]   [c] |   [d]
;;              |__________|

;; loop 5 START
;;       __________      __________    x->[/]
;;      |          |    |       y  |
;;      |          |    |       |  |
;;      v          |    v       v  |
;; v ->[*][/]  [*][*]  [*][*]->[*][*]
;;      |       ^\      |  |    |
;;      v       | v     v  |    v
;;     [a]      |[b]   [c] |   [d]
;;              |__________|
;; return y
;; (d c b a)

;; w
;; (d c b a)
;; v
;; (a)

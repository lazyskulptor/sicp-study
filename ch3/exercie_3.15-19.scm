;; Exercise 3.15-19


;; Exercise 3.15
;; GIVEN
(define x (list 'a 'b))
(define z1 (cons x x))

(define z2 (cons (list 'a 'b) (list 'a 'b)))

(define (set-to-wow! x)
  (set-car! (car x) 'wow)
  x)

;; z1-> [*][*]
;;       |  |
;;       | / 
;;       v  
;; x->  [*][*]->[*][/]
;;       |       |
;;       v       v
;;      [a]     [b]

;; z2-> [*][*]->[*][*]->[*][/]
;;       |       |       |     
;;       |       v       v     
;;       |      [a]     [b]    
;;       |       ^       ^
;;       |       |       |
;;       └----->[*][*]->[*][/]

;; WHEN
(set-to-wow! z1)

;; z1-> [*][*]
;;       |  |
;;       | / 
;;       v  
;; x->  [*][*]->[*][/]
;;       |       |
;;       v       v
;;     [wow]    [b]

;; WHEN
(set-to-wow! z2)

;; z2-> [*][*]->[*][*]->[*][/]
;;       |       |       |     
;;       |       v       v     
;;       |      [a]     [b]    
;;       |               ^
;;       |               |
;;       └----->[*][*]->[*][/]
;;               |
;;               v
;;             [wow]



;; Exercise 3.16

;; GIVEN
(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))


;; RETURN 3
(define t1 (cons 'a (cons 'b (cons 'c 'd))))

;; t1->  [*][*]->[*][*]->[*][*]->[d]
;;        |       |       |      
;;        v       v       v      
;;       [a]     [b]     [c]     

;; RETURN 4
(define t2-1 (cons 'a 'b))
(define t2 (cons t2-1 (cons t2-1 'c)))

;; t2->   [*][*]->[*][*]->[c]
;;         | ┌-----┘      
;;         |/             
;;         v              
;; t2-1-> [*][*]->[b]
;;         |     
;;         v     
;;        [a]    

;; RETURN 7
(define t3-1 (cons 'a 'b))
(define t3-2 (cons t2-1 t2-1))
(define t3 (cons t3-2 t3-2))

;; t3->   [*][*]
;;         | /
;;         |/             
;;         v              
;; t3-2-> [*][*]
;;         | /
;;         |/             
;;         v              
;; t3-1-> [*][*]->[b]
;;         |     
;;         v     
;;        [a]    

;; RETURN never
(define t4-1 (cons 'a 'b))
(define t4-2 (cons 'c 'd))
(define t4-3 (cons 'e t4-1))
(set-cdr! t4-1 t4-2)
(set-cdr! t4-2 t4-3)

;;          ┌------------------┐
;;          v                  |
;; t4-1 -> [*][*]->[*][*]->[*][*]
;;          |       |       |
;;          v       v       v
;;         [a]     [c]     [e]


;; Exercise 3.17

;; GIVEN
(define (append! x y)
  (cond ((null? x) y)
        (else (set-cdr! (last-pair x) y)
              x)))

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

;;

(define (count-pairs-2 x)
  (define (contains list e)
    (cond ((null? list) false)
          ((eq? (car (car list)) e) true)
          (else (contains (cdr list) e))))
  (let ((stack '()))
    (define (worker sub)

      ))
  (define (worker sub)
    (cond ((not (pair? sub)) 0)
          ((contains memo sub) (- (length memo) 1))
          (else
           (append! memo (cons sub '()))
           (display sub)
           (newline)
           (display memo)
           (newline)
           (- (+ 1
                 (worker (car sub) memo)
                 (worker (cdr sub) memo))
              (- (length memo) 1)))))
  (worker x (list (cons 'memo '()))))

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
          ((eq? (car list) e) true)
          (else (contains (cdr list) e))))
  (define memo (list 't))
  (define (worker sub)
    (when (and (pair? sub) (not (contains memo sub)))
      (append! memo (cons sub '()))
      (worker (car sub))
      (worker (cdr sub))))
  (worker x)
  (- (length memo) 1))


;; Exercie 3.18
(define (cycle? x)
  (define (worker head sub)
    (cond ((not (pair? sub)) false)
          ((or (worker head (car sub)) (worker head (cdr sub))) true)
          (else (worker (cdr sub) (cdr sub)))))
  (worker x x))


;; Exercise 3.19
;; In case there is cycle in (car x), I couldn't find the way.
;; The answer where cycle is in only next point.
;; Tortoise and Hare Algorithm.
(define (cycle? x)
  (define (worker head sub)
    (cond ((not (pair? head)) false)
          ((eq? head sub) true)
          ((null? (cdr sub)) false)
          (else (worker (cdr head) (cddr sub)))))
  (and (pair? x)
      (worker x (cdr x))))

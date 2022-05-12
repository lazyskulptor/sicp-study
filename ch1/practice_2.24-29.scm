;; sicp 2.24 - 2.29

;; 2.24
(list 1 (list 2 (list 3 4)))
;; (*|*)→(*|nil)
;;  ↓     ↓ 
;;  1   (2|*)→(*|nil)
;;             ↓
;;           (3|*)→(4|nil)

;; (list 1 (list 2 (list 3 4)))
;; |\
;; 1 (list 2 (list 3 4))
;;   |  \
;;   2   (list 3 4)
;;       |  \
;;       3   4


;; 2.25
(list 1 3 (list 5 7) 9)

(car (cdr (car (cdr (cdr (list 1 3 (list 5 7) 9))))))
;; 7

(list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7))))))

(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7))))))))))))))))))


;; 2.26
(define x (list 1 2 3))
(define y (list 4 5 6))

(append x y)
;; (1 2 3 4 5 6)

(cons x y)
;; ((1 2 3) 4 5 6)

(list x y)
;; ((1 2 3) (4 5 6))


;; 2.27
(define x (list (list 1 2) (list 3 4)))

(define (deep-reverse x)
  (cond ((null? x) nil)
        ((not (pair? x)) x)
        (else (reverse (cons (deep-reverse (car x))
                             (deep-reverse (cdr x)))))))


;; 2.28
(define (fringe t)
  (cond ((null? t) nil)
        ((not (pair? t)) t)
        (else (let ((l (fringe (car t))))
                (if (pair? l)
                    (append l (fringe (cdr t)))
                    (cons l (fringe (cdr t))))))))

;; 2.29

;; a
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch m)
  (car m))
(define (right-branch m)
  (car (cdr m)))
(define (branch-length b)
  (car b))
(define (branch-structure b)
  (car (cdr b)))


;; b
(define (total-weight m)
  (+ (branch-length (left-branch m))
     (branch-length (right-branch m))))


;; c
(define (balanced? m)
  (if (pair? m)
      (and (= (branch-length (left-branch m))
              (branch-length (right-branch m)))
           (balanced? (branch-structure (left-branch m)))
           (balanced? (branch-structure (right-branch m)))))
      #t))
  
;; d
;; right-branch와 branch-structure의 car 함수만 제거하면 된다.

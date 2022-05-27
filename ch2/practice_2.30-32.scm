;; sicp Exercise 2.30 - 2.32

;; 2.30
(define (square-tree t)
  (cond ((null? t) nil)
        ((not (pair? t)) (square t))
        (else (cons (square-tree (car t))
                    (square-tree (cdr t))))))

;; null검사가 map 안에 구현되어 있기 떄문에 null 체크는 필요가 없다.
;; 겉보기에 실제 item들만 관여하기 때문에 더 가독성이 좋아 보인다.
(define (square-tree t)
  (map (lambda (sub-t)
         (if (pair? sub-t)
             (square-tree sub-t)
             (square sub-t)))
       t))
  

(square-tree (list 1
                   (list 2
                         (list 3 4)
                         5)
                   (list 6 7)))


;; 2.31
(define (tree-map proc tree)
  (cond ((null? tree) nil)
        ((not (pair? tree))
         (proc tree))
        (else (cons (tree-map proc (car tree))
                    (tree-map proc (cdr tree))))))

(define (square-tree tree)
  (tree-map square tree))


;; 2.32
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map
                      (lambda (x) (append x (cons (car s) nil)))
                      rest)))))

;; 2.3.3 Example: Representing Sets

;; Sets as unordered lists
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

;; Exercise 2.59
;; O(m*n + n * (n + 1) / 2)
(define (union-set set1 set2)
  (if (null? set1)
      set2
      (union-set (cdr set1) (adjoin-set (car set1) set2))))

;; O(m * n + n')
(define (union-set set1 set2)
  (define (worker intersection sub-set1)
    (cond ((null? sub-set1) intersection)
          ((element-of-set? (car sub-set1) set2) (worker intersection (cdr sub-set1)))
          (else (worker (cons (car sub-set1) intersection) (cdr sub-set1)))))
  (define (union s1 s2)
    (if (null? s1)
        s2
        (union (cdr s1) (cons (car s1) s2))))
  (union (worker '() set1) set2))


;; Exercise 2.60
;; O(n) 하지만 n의 길이가 보장되지 않는다.
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

;; O(1) 비교 연산 없이 추가 하면된다.
(define (adjoin-set x set)
  (cons x set))

;; Big-O 값은 같지만 n의 값이 보장되지 않는다.
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))
;; O(n)
(define (union-set set1 set2)
  (if (null? set1)
      set2
      (union-set (cdr set1) (cons (car set1) set2))))

;;; Big-O 함수는 전체적으로 더 작다.


;; Sets as ordered lists
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))


;; Exercise 2.61
(define (adjoin-set x set)
  (define (attach set1 set2)
    (if (null? set1)
        set2
        (attach (cdr set1) (cons (car set1) set2))))
  (define (iter pre post)
    (cond ((null? post) (attach pre (cons x '())))
          ((= x (car post)) set)
          ((< x (car post)) (attach (cons x pre) post))
          (else (iter (cons (car post) pre) (cdr post)))))
  (iter '() set))
;; Unordered의 경우 element-of-set? 을 사용하면 길이 n의 값의 연산을 한다.
;; Ordered의 경우 각 element의 순서의 값을 연산한다.
;; 평균은 n * (n + 1) / (2 * n) => (n + 1)/2 이므로 Ordered의 경우 절반 정도의 연산을 덣 한다.

;; Exercise 2.62
(define (union-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (union-set (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (cons x1 (union-set (cdr set1) set2)))
              ((< x2 x1)
               (cons x2 (union-set set1 (cdr set2))))))))


;; Sets as binary trees
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))
                     


;; Exercise 2.63
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

;; a.

(tree->list-1 (make-tree 7 (make-tree 3 (make-tree 1 nil nil) (make-tree 5 nil nil)) (make-tree 9 nil (make-tree 11 nil nil))))
;Value: (1 3 5 7 9 11)
(tree->list-1 (make-tree 3 (make-tree 1 nil nil) (make-tree 7 (make-tree 5 nil nil) (make-tree 9 nil (make-tree 11 nil nil)))))
;Value: (1 3 5 7 9 11)
(tree->list-1 (make-tree 5 (make-tree 3 (make-tree 1 nil nil) nil) (make-tree 9 (make-tree 7 nil nil) (make-tree 11 nil nil))))
;Value: (1 3 5 7 9 11)

(tree->list-2 (make-tree 7 (make-tree 3 (make-tree 1 nil nil) (make-tree 5 nil nil)) (make-tree 9 nil (make-tree 11 nil nil))))
;Value: (1 3 5 7 9 11)
(tree->list-2 (make-tree 3 (make-tree 1 nil nil) (make-tree 7 (make-tree 5 nil nil) (make-tree 9 nil (make-tree 11 nil nil)))))
;Value: (1 3 5 7 9 11)
(tree->list-2 (make-tree 5 (make-tree 3 (make-tree 1 nil nil) nil) (make-tree 9 (make-tree 7 nil nil) (make-tree 11 nil nil))))
;Value: (1 3 5 7 9 11)

;; b.




;; case 1
;; (tree->list-1 tree)
;; (append (tree->list-1 left) (cons entry (tree->list-1 right)))


;; case 2
;; (tree->list-2 tree)
;; (copy-to-list tree result)
;; (copy-to-list left (cons entry (copy-to-list right result)))

;; 자라는 모양은 같다. 하지만 append 에서 n번의 loop가 한 번 더 돌기 때문에 case2가 더 빠르다.


;; 2.64
(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

;; a
;; 중앙값은 this-entry로 make-tree의 entry값이 된다.
;; 중앙값의 좌, 우측은 partial-tree의 값을 등록한다.

;; ( 1 3 5 7 9 11)

;;          5
;;          ^
;;       /     \
;;      1       9
;;      ^       ^
;;       \     / \
;;        3   7  11

;; b.
;; n=1   (list->tree (list 1)) => 3번
;; (partial-tree (1) 1)
;; (make-tree (partial-tree (1) 0) 1 (partial-tree '() 0))

;; n=2  (list->tree (list 1 2)) => 5번
;; (partial-tree (1 2) 2)
;; (make-tree (partial-tree (1 2) 0) 1 (partial-tree (2) 1))

;; n=3  (list->tree (list 1 2 3)) => 7번
;; (partial-tree (1 2 3) 3)
;; (make-tree (partial-tree (1 2 3) 1) 2 (partial-tree (3) 1))

;; n=4  (list->tree (list 1 2 3 4)) => 9번
;; (partial-tree (1 2 3 4) 4)
;; (make-tree (partial-tree (1 2 3 4) 1) 2 (partial-tree (3 4) 2))

;; n=5  (list->tree (list 1 2 3 4 5)) => 11번
;; (partial-tree (1 2 3 4 5) 5)
;; (make-tree (partial-tree (1 2 3 4 5) 2) 2 (partial-tree (3 4) 2))

;; n이 짝수일 땐 
;; f(n) = 2 * f(n/2) - 1

;; n이 홀 수일 때
;; f(n) = 2 * f((n - 1)/2) + 1
;; f(n) ≈ 2n 


;; 2.65
(define (union-set s1 s2)
  (define (union set1 set2)
    (if (or (null? set1) (null? set2))
        '()
        (let ((x1 (car set1)) (x2 (car set2)))
          (cond ((= x1 x2)
                (cons x1
                      (union (cdr set1)
                                        (cdr set2))))
                ((< x1 x2)
                (cons x1 (union (cdr set1) set2)))
                ((< x2 x1)
                (cons x2 (union set1 (cdr set2))))))))
  (let ((l1 (tree->list-2 s1))
        (l2 (tree->list-2 s2)))
    (list->tree (union l1 l2))))

(define (intersection-set s1 s2)
  (define (intersection set1 set2)
    (if (or (null? set1) (null? set2))
        '()
        (let ((x1 (car set1)) (x2 (car set2)))
          (cond ((= x1 x2)
                (cons x1
                      (intersection (cdr set1)
                                        (cdr set2))))
                ((< x1 x2)
                (intersection (cdr set1) set2))
                ((< x2 x1)
                (intersection set1 (cdr set2)))))))
  (let ((l1 (tree->list-2 s1))
        (l2 (tree->list-2 s2)))
    (list->tree (intersection l1 l2))))


;; Sets and information retrieval
(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((equal? given-key (key (car set-of-records)))
         (car set-of-records))
        (else (lookup given-key (cdr set-of-records)))))

;; 2.66

;; binary tree
(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((equal? given-key (key (car set-of-records))) (car set-of-records))
        ((< given-key (key (car set-of-records)))
         (lookup given-key (left-branch set-of-records)))
        ((> given-key (key (car set-of-records)))
         (lookup given-key (right-branch set-of-records)))))

;; ordered list
(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((= given-key (key (car set-of-records)))
         (car set-of-records))
        ((< given-key (key (car set-of-records))) false)
        (else (lookup given-key (cdr set-of-records)))))

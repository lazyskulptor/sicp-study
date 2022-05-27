;; 2.3.4 Example: Huffman Encoding Trees

;; Representing Huffman trees
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))


(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))


;; The decoding procedure
(define (decode msg tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 msg tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))


;; Sets of weighted elements
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

;; 2.67
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(decode sample-message sample-tree)
;Value: (a d a b b c a)


;; 2.68
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))


(define (encode-symbol msg tree)
  (define (worker branch)
    (if (leaf? branch) (equal? msg (symbol-leaf branch))
        (let ((left (worker (left-branch branch))))
          (if left
              (if (pair? left) (cons 0 left) (cons 0 '()))
              (let ((right (worker (right-branch branch))))
                (if right
                    (if (pair? right) (cons 1 right) (cons 1 '()))
                    false))))))
  (let ((rs (worker tree)))
    (if rs
        rs
        (error "Not Defiend" msg))))


;; 2.69 
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(make-leaf-set '((A 4) (B 2) (C 1) (D 1)))
;Value: ((leaf d 1) (leaf c 1) (leaf b 2) (leaf a 4))

(define (successive-merge leaf-set)
  (define (w l) (if (leaf? l) (weight-leaf l) (weight l)))
  (define (min-pair subs mins rest)
    (cond ((null? subs) (cons (make-code-tree (cdr mins) (car mins)) rest))
          ((null? mins) (min-pair (cdr (cdr subs)) (if (> (w (car subs)) (w (cadr subs)))
                                                       (cons (cadr subs) (car subs))
                                                       (cons (car subs) (cadr subs))) (cdr (cdr subs))))
          ((< (w (car subs)) (w (car mins)))
           (min-pair (cdr subs) (cons (car subs) (car mins)) (cons (cdr mins) rest)))
          (else (min-pair (cdr subs) mins rest))))
  (define (iter rs)
    (if (< (length rs) 2)
        rs
        (iter (min-pair rs '() '()))))
  (car (iter leaf-set)))

(generate-huffman-tree '((A 4) (B 2) (C 1) (D 1)))
;Value: ((leaf a 4) ((leaf b 2) ((leaf c 1) (leaf d 1) (c d) 2) (b c d) 4) (a b c d) 8)


;; 2.70
(define rock-tree (generate-huffman-tree '((A 2) (NA 16) (BOOM 1) (SHA 3) (GET 2) (YIP 9) (JOB 2) (WAH 1))))
;Value: (((((((leaf job 2) ((leaf boom 1) (leaf wah 1) (boom wah) 2) (job boom wah) 4) (leaf get 2) (job boom wah get) 6) (leaf a 2) (job boom wah get a) 8) (leaf sha 3) (job boom wah get a sha) 11) (leaf yip 9) (job boom wah get a sha yip) 20) (leaf na 16) (job boom wah get a sha yip na) 36)


(encode '(Get a job
              Sha na na na na na na na na
              Get a job
              Sha na na na na na na na na
              Wah yip yip yip yip yip yip yip yip yip
              Sha boom) rock-tree)

;; 87 bits  huffman-tree를 사용할 땐 87bits. 고정 8bit를 사용할 땐 288(36 * 8)bits가 필요하다.
;Value: (0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 0 0 0 0 0 1 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 0 1 0 0 0 0 0 1 0)


;; 2.71
(generate-huffman-tree '((A 1) (B 2) (C 4) (D 8) (E 16)))
;Value: ((leaf e 16) ((leaf d 8) ((leaf c 4) ((leaf b 2) (leaf a 1) (b a) 3) (c b a) 7) (d c b a) 15) (e d c b a) 31)

;;        ((e d c b a) 31)
;;       /                \
;; (E 16)                  ((d c b a) 15)
;;                        /              \
;;                   (D 8)                ((c b a) 7)
;;                                       /           \
;;                                  (C 4)             ((B A) 3)
;;                                                   /         \
;;                                              (B 2)           (A 1)


(encode '(E) (generate-huffman-tree '((A 1) (B 2) (C 4) (D 8) (E 16))))
;Value: (0)
(encode '(A) (generate-huffman-tree '((A 1) (B 2) (C 4) (D 8) (E 16))))
;Value: (1 1 1 1)

(generate-huffman-tree '((A 1) (B 2) (C 4) (D 8) (E 16) (F 32) (G 64) (H 128) (I 256) (J 512)))
;Value: ((leaf j 512) ((leaf i 256) ((leaf h 128) ((leaf g 64) ((leaf f 32) ((leaf e 16) ((leaf d 8) ((leaf c 4) ((leaf b 2) (leaf a 1) (b a) 3) (c b a) 7) (d c b a) 15) (e d c b a) 31) (f e d c b a) 63) (g f e d c b a) 127) (h g f e d c b a) 255) (i h g f e d c b a) 511) (j i h g f e d c b a) 1023)


(encode '(A) (generate-huffman-tree '((A 1) (B 2) (C 4) (D 8) (E 16) (F 32) (G 64) (H 128) (I 256) (J 512))))
;Value: (1 1 1 1 1 1 1 1 1)

(encode '(J) (generate-huffman-tree '((A 1) (B 2) (C 4) (D 8) (E 16) (F 32) (G 64) (H 128) (I 256) (J 512))))
;Value: (0)

;; 2.72

;; most frequent
;;  (encode-symbol '(E) tree)
;;  (worker 'E tree)
;;  (cons 0 (worker 'E (left-branch tree))
;;  (cons 0 (equal? (symbol-leaf (left-branch tree))))
;;  (0)
;; 항상 2회 불림 O(1) 


;; least frequent : (E, 2) (D, 5), (C, 8), (B, 11) (A, 14)
;; (encode-symbol ('A) tree)
;; (worker 'A tree)
;; (if (worker 'A (left-branch tree)) ;; 2회 호출
;;   (cons 0 ...)                     ;; E의 경우 끝남
;;   (cons 1 (worker 'A (right-branch tree)))) 

;; (cons 1 (worker 'A tree`)) ;; 1회 호출
;; (cons 1
;  (if (worker 'A (left-branch tree`)) ;; 2회 호출
;;   (cons 0 ...)                     ;; D의 경우 끝남
;;   (cons 1 (worker 'A (right-branch tree`)))))

;; (cons 1 (cons 1 (worker 'A tree``))) ;; 1회 호출
;; (cons 1 (cons 1
;; (if (worker 'A (left-branch tree``)) ;; 2회 호출
;;   (cons 0 ...)                     ;; C의 경우 끝남
;;   (cons 1 (worker 'A (right-branch tree``)))))

;; (cons 1 (cons 1 (cons 1 (worker 'A tree```))) ;; 1회 호출
;; (cons 1 (cons 1 (cons 1
;; (if (worker 'A (left-branch tree```)) ;; 2회 호출
;;   (cons 0 ...)                     ;; B의 경우 끝남
;;   (cons 1 (worker 'A (right-branch tree```)))))

;; (cons 1 (cons 1 (cons 1 (cons 1 (worker 'A tree````))) ;; 1회 호출
;; (cons 1 (cons 1 (cons 1 (cons 1
;; (if (worker 'A (left-branch tree````)) ;; 2회 호출
;;   (cons 0 ...)                     ;; A의 경우 끝남
;;   (cons 1 (worker 'A (right-branch tree````)))))

;; g(n) = 3n - 1

;; sicp 2.40 - 2.43

;; 2.40
(define (unique-pairs n)
  (flatmap (lambda (x)
             (map (lambda (y) (list x y)) (enumerate-interval 1 (- x 1))))
           (enumerate-interval 1 n)))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

;; 2.41
(define (unique-triples n)
  (flatmap (lambda (x)
             (flatmap (lambda (y)
                        (map (lambda (z) (list z y x)) (enumerate-interval 1 (- y 1))))
                      (enumerate-interval 1 (- x 1))))
           (enumerate-interval 1 n)))

(define (ordered-triples n s)
  (filter (lambda (x) (= s (+ (car x) (cadr x) (cadr (cdr x))))) (unique-triples n)))


;; 2.42
(define (queens board-size)
  (define (queens-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queens-cols (- k 1))))))
  (queens-cols board-size))

(define (adjoin-position new-row k rest-of-queens)
  (append rest-of-queens (cons new-row nil)))

(define empty-board nil)

(define (safe? k positions)
  (define (woker subs cnt sub-pos)
    (if (= cnt 0)
        (let ((last (car sub-pos)))
          (null? (filter (lambda (pos)
                           (or (= last (car pos)) (= last (cadr pos))
                               (= last (/ (+ (car pos) (cadr pos)) 2)))) subs)))
        (woker (cons (list (- (car sub-pos) cnt) (+ (car sub-pos) cnt)) subs)
               (- cnt 1)
               (cdr sub-pos))))
  (woker nil (- k 1) positions))

;; 2.43
(define (queens board-size)
  (define (queens-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (new-row) ;; 두 위치가 바뀜
            (map (lambda (rest-of-queens) ;;  
                   (adjoin-position new-row k rest-of-queens))
                 (queens-cols (- k 1))))
          (enumerate-interval 1 board-size)))))
  (queens-cols board-size))

;; case 2.42
;; (qeens-cols k)
;; k*(qeens-cols (- k 1))
;; k*(k-1)*(qeens-cols (- k 2))
;; ...
;; k!

;; case 2.43
;; (qeens-cols k)이 n승번 더 불리기 때문에 T^board-size 단위로 올라감

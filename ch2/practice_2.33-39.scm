;; sicp Exercise 2.33 - 2.39

;; 2.33
(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))


;; 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

;; 2.35
(define (count-leaves t)
  (accumulate + 0 (map (lambda (x) 1) (enumerate-tree t))))


;; 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map (lambda (x) (car x)) seqs))
            (accumulate-n op init (map (lambda (x) (cdr x)) seqs)))))

(accumulate-n + 0 (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
;Value: (22 26 30)

;; 2.37
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))

(define (transpose mat)
  (accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (matrix-*-vector cols x)) m)))


;; 2.38
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (fold-right op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op (car rest) result)
              (cdr rest))))
  (iter initial sequence))


(fold-right / 1 (list 1 2 3))
;Value: 3/2
(fold-left / 1 (list 1 2 3))
;Value: 1/6
(fold-right list nil (list 1 2 3))
;; (1 (2 (3 ())))
(fold-left list nil (list 1 2 3))
;; (((() 1) 2) 3)


;; 2.39
(define (reverse sequence)
  (fold-right (lambda (x y) (cons x y)) nil sequence))

(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))

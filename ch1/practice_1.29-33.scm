;; sicp

;; 1.29

(define (simpson-rule f a b n)
  (define h (/ (- b a) n))
  (define (inc x) (+ x 1))
  (define (y k)
    (f (+ a (* k h))))
  (define (term k)
    (* (cond ((odd? k) 4)
             ((or (= k 0) (= k n)) 1)
             ((even? k) 2))
       (y k)))
  (/ (* h (sum term 0 inc n)) 3.0))


;; 1.30

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

;; 1.31.a

(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))

(define (factorial n)
  (define (identity x) x)
  (define (inc x) (+ x 1))
  (product identity 1 inc n))

(define (pi-approximate n)
  (define (pi-term x) (* (/ (- x 1) x) (/ (+ x 1) x)))
  (define (next x) (+ x 2))
  (* 4.0 (product pi-term 3 next n)))

;; 1.31.b
(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

;; 1.32.b

(define (accumulate-iter combiner null-value term a next b)
  (define (iter result a)
    (if (> a b)
        result
        (iter (combiner result (term a) (next a)))))
  (iter null-value a))


;; 1.33
(define (accumulate-filter combiner null-value term a next b predicate)
  (if (> a b)
      null-value
      (let ((v (term a)))
        (combiner (if (predicate a) (term a) null-value)
                  (accumulate-filter combiner null-value term (next a) next b predicate)))))

(define (sum-prime a b)
  (define (square x) (* x x))
  (define (inc x) (+ x 1))
  (accumulate-filter + 0 square a inc b prime?))

(define (product-prime n)
  (define (identity x) x)
  (define (inc x) (+ x 1))
  (accumulate-filter * 1 identity 2 inc (- n 1) gcd))

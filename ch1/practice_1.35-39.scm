;; sicp practice 1.35 - 1.39

;; 1.35
(define (golden-ratio)
  (fixed-point (lambda (y) (+ 1 (/ 1 y)))
               1.0))
  
;; 1.36
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display next)
      (newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (x^x)
  (fixed-point (lambda (x) (/ (log 1000) (log x)))
               2.0))
  

;; 1.37.a
(define (cont-frac n d k)
  (define (recur i)
    (if (> i k)
        0
        (/ (n i) (+ (d i) (recur (+ i 1))))))
  (recur 1))

;; 11 번째부터 네번째 값이 바뀌지 않음
;; 1.
;; .5
;; .6666666666666666
;; .6000000000000001
;; .625
;; .6153846153846154
;; .6190476190476191
;; .6176470588235294
;; .6181818181818182
;; .6179775280898876
;; .6180555555555556
;; .6180257510729613
;; .6180371352785146
;; .6180327868852459
;; .6180344478216819
;; .6180338134001252
;; .6180340557275542

;; 1.37.b
(define (cont-frac-iter n d k)
  (define (iter rs cnt-down)
    (if (< cnt-down 1)
        rs
        (iter (/ (n cnt-down) (+ (d cnt-down) rs)) (- cnt-down 1))))
  (iter 0 k))


;; 1.38
(define (euler-exp k)
  (define (d a)
    (if (= (remainder (+ a 1) 3) 0)
        (* 2 (/ (+ a 1) 3))
        1))
  (cont-frac-iter (lambda (i) 1.0) d k))

;; 1.39
(define (tan-cf x k)
  (let ((v (* -1 x x)))
    (cont-frac-iter (lambda (i) (if (= i 1) x v))
                    (lambda (i) (- (* 2 i) 1)) k)))

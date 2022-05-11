;; sicp 2.7 - 2.16


(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                    (max p1 p2 p3 p4))))


(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))


;; 2.7
(define (make-interval a b) (cons a b))

(define (upper-bound r) (cdr r))

(define (lower-bound r) (car r))

;; 2.8
(define (sub-interval a b)
  (make-interval (- (lower-bound a) (upper-bound b))
                 (- (upper-bound a) (lower-bound b))))

;; 2.9
(define (width-interval r)
  (/ (- (upper-bound r) (lower-bound r)) 2))

(define (sub-width-1 r1 r2)
  (sub-interval (width-interval r1) (width-interval r1)))

(define (sub-width-2 w1 w2)
  (- w1 w2))

;; (sub-width-1 (make-interval a b) (make-interval c d))
;; (sub-width-1 (cons a b) (cons c d))
;; (sub-interval (width-interval (cons a b)) (width-interval (cons c d)))
;; (sub-interval (/ (- (upper-bound (cons a b)) (lower-bound (cons a b))) 2)
;;               (/ (- (upper-bound (cons c d)) (lower-bound (cons c d))) 2))


;; (sub-width-2 (width-interval (make-interval a b)) (width-interval (make-interval c d)))
;; (sub-width-2 (width-interval (cons a b)) (width-interval (cons c d)))
;; (sub-width-2 (/ (- (upper-bound (cons a b)) (lower-bound (cons a b))) 2)
;;              (/ (- (upper-bound (cons c d)) (lower-bound (cons c d))) 2))

;; 두개의 결과값이 같다

(define r1 (make-interval 17 21))
(define r2 (make-interval 63 65))

(* (width-interval r1) (width-interval r2))
;Value: 2

(* (width-interval (mul-interval r1 r2)))
;Value: 147

(/ (width-interval r1) (width-interval r2))
;Value 2

(* (width-interval (div-interval r1 r2)))
;Value: .03589743589743588

;; 2.10
(define (div-interval x y)
  (let ((upper-x (upper-bound x)) (lower-x (lower-bound x))
        (upper-y (upper-bound y)) (lower-y (lower-bound y)))
    (if (or (and (< 0 lower-x) (> 0 upper-x))
            (and (> 0 lower-x) (< 0 upper-x))
            (and (< 0 lower-y) (> 0 upper-y))
            (and (> 0 lower-y) (< 0 upper-y)))
        (error "Intervals should be bigger than zero")
        (mul-interval x
                (make-interval (/ 1.0 upper)
                               (/ 1.0 lower))))))

;; 2.11
(define (mul-interval x y)
  (let ((upper-x (upper-bound x)) (lower-x (lower-bound x))
        (upper-y (upper-bound y)) (lower-y (lower-bound y))
        (pos? (lambda (l u) (cond ((and (< l 0) (u > 0))
                                   (error "Intervals should be bigger than zero"))
                                  ((and (> l 0) (> u 0)) #t)
                                  ((and (< l 0) (< u 0)) #f)))))
    (cond ((and (pos? lower-x upper-x) (pos? lower-y upper-y))
           (make-interval (* lower-x lower-y) (* upper-x upper-y)))
          ((and (pos? lower-x upper-x) (not (pos? lower-y upper-y)))
           (make-interval (* upper-x lower-y) (* lower-x upper-y)))
          ((and (not (pos? lower-x upper-x)) (pos? lower-y upper-y))
           (make-interval (* upper-x lower-y) (* lower-x upper-y)))
          ((and (not (pos? lower-x upper-x)) (not (pos? lower-y upper-y)))
           (make-interval (* lower-x lower-y) (* upper-x upper-y))))))

;; 2.12

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))


(define (make-center-percent center percent-tolerance)
  (let ((t (* center (/ percent-tolerance 100))))
    (make-center-width center t)))

(define (percent i)
  (* (/ (width i) (center i)) 100))

;; 2.13


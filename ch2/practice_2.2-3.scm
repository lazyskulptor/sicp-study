;; sicp 2.2 - 2.3


;; 2.2

(define(print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))
  
(define (make-segment l r)
  (cons l r))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (midpoint-segment s)
  (define (average x y)
    (/ (+ x y) 2.0))
  (make-point (average (x-point (start-segment s)) (x-point (end-segment s)))
              (average (y-point (start-segment s)) (y-point (end-segment s)))))



;; 2.3 http://community.schemewiki.org/?sicp-ex-2.3 내용 그대로 가져옴
(define (perimeter-rect r)
  (* 2 (+ (width-rect r) (height-rect r))))

(define (area-rect r)
  (* (width-rect r) (height-rect r)))

(define (make-rect bottom-left top-right)
  (cons lb rt))


(define (bottom-left rect) (car rect)) 
(define (bottom-right rect) 
  (make-point (x-point (cdr rect)) 
              (y-point (car rect)))) 
(define (top-left rect) 
  (make-point (x-point (car rect)) 
              (y-point (cdr rect)))) 
(define (top-right rect) (cdr rect)) 

(define (width-rect rect) 
  (abs (- (x-point (bottom-left rect)) 
          (x-point (bottom-right rect))))) 
(define (height-rect rect) 
  (abs (- (y-point (bottom-left rect)) 
          (y-point (top-left rect))))) 

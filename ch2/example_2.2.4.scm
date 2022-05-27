;; 2.2.4 Example: A Picture Language
(define wave2 (beside wave (flip-vert wave)))
(define wave4 (below wave2 wave2))

(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

(define wave4 (flipped-pairs wave))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))


;; Exercise 2.44
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

;; Hihger-order operations
(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) br (painter))))
      (below bottom top))))

(define (flipped-pairs painter)
  (let ((combine4 (square-of-four identity flip-vert
                                  identity flip-vert)))
    (combine4 painter)))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))


;; Exercise 2.45
(define right-split (split beside below))
(define up-split (split below beside))

(define (split side1 side2)
  (define (worker painter n)
    (if (= n 0)
        painter
        (let ((smaller (woker painter (- n 1))))
          (side1 painter (side2 smaller smaller)))))
  (lambda (painter n)
    (woker painter n)))


;; Frames
(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (scor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

;; Exercise 2.46
(define (make-vect x y)
  (cons x y))
(define (xcor-vect c)
  (car c))
(define (ycor-vect c)
  (cdr c))

(define (add-vect c1 c2)
  (make-vect (+ (xcor-vect c1) (xcor-vect c2))
             (+ (ycor-vect c1) (ycor-vect c2))))

(define (sub-vect c1 c2)
  (make-vect (- (xcor-vect c1) (xcor-vect c2))
             (- (ycor-vect c1) (ycor-vect c2))))

(define (scale-vect s c)
  (make-vect (* s (xcor-vect c))
             (* s (ycor-vect c1))))


;; Exercise 2.47
;; case 1
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
(define (origin-vect f)
  (car f))
(define (edge1-vect f)
  (cadr f))
(define (edge2-vect f)
  (cadr (cdr f)))

;; case 2
(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
(define (origin-vect f)
  (car f))
(define (edge1-vect f)
  (car (cdr f)))
(define (edge2-vect f)
  (cdr (cdr f)))

;; Painters
(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))


;; 2.48
(define (make-segment start-point end-point)
  (cons start-point end-point))
(define (start-segment s)
  (car s))
(define (end-segment s)
  (cdr s))


;; 2.49
;; a.
(define (frame-outline f)
  (segments->painter (list (make-segment (car f) (cadr f))
                           (make-segment (cadr f) (cadr (cdr f)))
                           (make-segment (cadr (cdr f)) (sub-vect (add-vect (cadr f) (cadr (cdr f))) (car f)))
                           (make-segment (sub-vect (add-vect (cadr f) (cadr (cdr f))) (car f)) (car f)))))

;; b.
(define (paint-x f)
  (segments->painter (list (make-segment (car f) (sub-vect (add-vect (cadr f) (cadr (cdr f))) (car f)))))
  (segments->painter (list (make-segment (cadr f) (cadr (cdr f))))))

;; c. ...
                     


;; Transforming and combining painters
(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (sub-vect (m corner1) new-origin)
                     (sub-vect (m corner2) new-origin)))))))

(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (shrink-to-upper-right painter)
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 1.0 0.5)
                     (make-vect 0.5 1.0)))


(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              split-point
                              (make-vect 0.0 1.0)))
          (paint-right
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.0)
                              (make-vect 0..5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

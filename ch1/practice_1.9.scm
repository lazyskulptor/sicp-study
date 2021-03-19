#lang sicp

;(define (+ a b)
;  (if (= a 0)
;    b
;    (inc (+ (dec a) b))))

;; recursive process 형태로 호출된다.
; (+ 4 5)
; (if (= 4 0) 5 (inc (+ (dec 4) 5)))

; (inc (+ (3) 5)

; (inc 
;   (if (= 3 0) 5 (inc (+ (dec 3) 5)))
; )

; (inc 
;   ((inc (+ (2) 5)))
; )

; (inc 
;   ((inc 
;      (if (= 2 0) 5 (inc (+ (dec 2) 5)))
;   ))
; )

; (inc 
;   ((inc 
;      ((inc (+ (1) 5)))
;   ))
; )

; (inc 
;   ((inc 
;      ((inc 
;         (if (= 1 0) 5 (inc (+ (dec 1) 5)))
;      ))
;   ))
; )

; (inc 
;   ((inc 
;      ((inc 
;         ((inc (+ (0) 5)))
;      ))
;   ))
; )

; (inc 
;   ((inc 
;      ((inc 
;         ((inc 
;            (if (= 0 0) 5 (inc (+ (dec 0) 5)))
;         ))
;      ))
;   ))
; )

; (inc 
;   ((inc 
;      ((inc 
;         ((inc 
;            (5)
;         ))
;      ))
;   ))
; )
;;  6


;;  두번째는 재귀로 호출되지만 iterative process 로 호출된다.
;(define (+ a b)
;  (if (= a 0)
;    b
;    (+ (dec a) (inc b))))

; (+ 4 5)
;
; (if (= 4 0) 5 (+ (dec 4) (inc 5)))
; ((+ (3) (6)))

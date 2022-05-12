;; sicp Exercise 2.21-2.23

;; 2.21

(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list (cdr items)))))

(define (square-list items)
  (map square items))

;; 2.22

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items nil))

; Why not? => (cons 1 (cons 2 (cons 3 nil))) 는 applicative order의 적용으로 내부,(cons 3 nil)부터 평가된다. 하지만 위 함수는 리스트의 순서대로 구조체를 만들기 때문에 (cons 1 nil)이 먼저 평가되어 역순으로 출력된다.
    

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items nil))

; Why not? => (cdr things)가 cons로 만들어진 구조체이기 때문에 (cons 1 (cons 2 nil)) 형태가 아닌 (cons (cons 1) ...) 형태가 됨으로 작동하지 정상작동 하지 않는다.
; 이전에 만든 append 함수를 이용해야 한다


(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (append answer (square (car things))))))
  (iter items nil))

;; 2.23

(define (for-each proc items)
  (cond ((null? items) nil)
        (else (proc (car items))
              (for-each proc (cdr items)))))

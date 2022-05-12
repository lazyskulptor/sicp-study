;; Exercise 2.17

;; 2.17

(define (last-pair items)
  (if (null? (cdr items))
      (car items)
      (last-pair (cdr items))))

;; 2.18
(define (reverse items) 
  (if (null? (cdr items)) 
      items 
      (append (reverse (cdr items)) 
              (cons (car items) '())))) 

(define (reverse items) 
  (define (iter items result) 
    (if (null? items) 
        result 
        (iter (cdr items) (cons (car items) result)))) 

  (iter items '())) 


;; 2.19

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

(define (first-denomination coin-values)
  (car coin-values))

(define (except-first-denomination coin-values)
  (cdr coin-values))

(define (no-more? coin-values)
  (null? coin-values))

;; coin들의 순서가 바뀌면 정상적으로 작동하지 않는다. 큰 숫자의 동전 개수를 먼저 세는 방식이기 때문에 순서가 바뀌면 뒤쪽에 있는 큰 숫자의 동전으로 거스를 수 있는 개수가 부정확해진다.

;; 2.20
(define (same-parity . params)
  (define filter
    (if (even? (car params))
        even?
        odd?))
  (define (worker l1 l2)
    (if (null? l2)
        l1
        (let ((item (car l2)))
          (worker
           (if (filter item)
               (append l1 (cons item '()))
               l1)
           (cdr l2)))))
  (worker '() params))

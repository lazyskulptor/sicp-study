;; sicp 2.53 2.55

(define (memq item x)
  (cond ((null? x) x)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

;; 2.53

(list 'a 'b 'c)
;Value: (a b c)

(list (list 'george))
;Value: ((george))

(cdr '((x1 x2) (y1 y2)))
;Value: ((y1 y2))

(cadr '((x1 x2) (y1 y2)))
;Value: (y1 y2)

(pair? (car '(a short list)))
;Value: #f

(memq 'red '((red shoes) (blue socks)))
;Value: ()

(memq 'red '(red shoes blue socks))
;Value: (red shoes blue socks)


;; 2.54
(define (equal? a b)
  (cond ((and (not (pair? a)) (not (pair? b))) (eq? a b))
        ((and (pair? a) (pair? b))
         (and (equal? (car a) (car b)) (equal? (cdr a) (cdr b))))
        (else false)))


;; 2.55

''abracadabra 
;Value: (quote abracadabra)

;; car 는 list의 첫 element를 주기 떄문에 quote 값이 나옴

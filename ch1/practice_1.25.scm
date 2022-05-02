#lang sicp

;; 
(define (expmod-orignal base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))


(define (expmod-2 base exp m)
  (remainder (fast-expt base exp) m))

;; 정의상 틀리진 않기 때문에 방법은 맞지만
;; fast-expt의 지수를 계산한 값을 갖기 때문에 메모리가 많이 필요한 반면
;; expmod-original 은 각주 46번의 내용을 사용하기 때문에 적은 메모리로 같은 결과를 낼 수 있다

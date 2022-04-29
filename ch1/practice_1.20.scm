#lang sicp

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;; (gcd 206 40)
;; normal order => 18 steps
(gcd 206 40)

(gcd 40 (remainder 206 40) 

(if (= 40 0) ;; 1 called
    206
    (gcd 40 (remainder 206 40)))

(if (= (remainder 206 40) 0) 
    40
    (gcd (remainder 206 40)
         (remainder 40 (remainder 206 40))))

(if (= 6 0) ;; 2 called
    40
    (gcd (remainder 206 40) (remainder 40 (remainder 206 40))))

(gcd (remainder 206 40) (remainder 40 (remainder 206 40)))

(if (= (remainder 40 (remainder 206 40)) 0) 
    (remainder 206 40)
    (gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))

(if (= (remainder 40 6) 0) ;; 3 called
    (remainder 206 40)
    (gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))

(if (= 4 0) ;; 4 called
    (remainder 206 40)
    (gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))

(gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))

(if (= (remainder 6 (remainder 40 (remainder 206 40))) 0)
    (remainder 40 (remainder 206 40))
    (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))))

(if (= (remainder 6 (remainder 40 6)) 0) ;; 5 called
    (remainder 40 (remainder 206 40))
    (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))))

(if (= (remainder 6 4) 0) ;; 6 called
    (remainder 40 (remainder 206 40))
    (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))))

(if (= 2 0) ;; 7 called
    (remainder 40 (remainder 206 40))
    (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))))

(gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))

(gcd (remainder 6 (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))) ;; 8 called

(gcd (remainder 6 (remainder 40 6)) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))) ;; 9 called

(gcd (remainder 6 4) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))) ;; 10 called

(gcd 2 (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))) ;; 11 called

(gcd 2 (remainder (remainder 40 6) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))) ;; 12 called

(gcd 2 (remainder 4 (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))) ;; 13 called

(gcd 2 (remainder 4 (remainder 6 (remainder 40 (remainder 206 40))))) ;; 14 called

(gcd 2 (remainder 4 (remainder 6 (remainder 40 6)))) ;; 15

(gcd 2 (remainder 4 (remainder 6 4))) ;; 16

(gcd 2 (remainder 4 2)) ;; 17

(gcd 2 0) ;;18

(if (= 0 0)
    2
    (gcd 0 (remainder 2 0)))

2


;; applicative order => 4 times

(gcd 40 (remainder 206 40))
(gcd 40 6) ;; 1 called
(gcd 6 (remainder 40 6))
(gcd 6 4) ;; 2 called
(gcd 4 (remainder 6 4))
(gcd 4 2) ;; 3 called
(gcd 2 (remainder 4 2))
(gcd 2 0) ;; 4 called
2

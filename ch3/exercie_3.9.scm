;; Exercise 3.9


;; Case 1
;; GIVEN
(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

;; Step 1
;; (            Gobal Env            )
;;   ^   
;;   |   
;; ( 6 ) 

;; Step 2
;; (            Gobal Env            )
;;   ^     ^  
;;   |     |  
;; ( 6 ) ( 5 )

;; Step 3 
;; (            Gobal Env            )
;;   ^     ^     ^  
;;   |     |     |  
;; ( 6 ) ( 5 ) ( 4 )

;; Step 4
;; (            Gobal Env            )
;;   ^     ^     ^     ^
;;   |     |     |     |
;; ( 6 ) ( 5 ) ( 4 ) ( 3 )

;; Step 5
;; (            Gobal Env            )
;;   ^     ^     ^     ^     ^
;;   |     |     |     |     |
;; ( 6 ) ( 5 ) ( 4 ) ( 3 ) ( 2 )

;; Step 6
;; (            Gobal Env            )
;;   ^     ^     ^     ^     ^
;;   |     |     |     |     |
;; ( 6 ) ( 5 ) ( 4 ) ( 3 ) ( 2 )

;; Step 7
;; (            Gobal Env            )
;;   ^     ^     ^     ^     ^     ^
;;   |     |     |     |     |     |
;; ( 6 ) ( 5 ) ( 4 ) ( 3 ) ( 2 ) ( 1 )


;; Case 2
;; GIVEN
(define (factorial n)
  (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter 9* counter product)
      (+ counter 1)
      (max-count)))


;; Step 1
;; (            Gobal Env            )
;;   ^   
;;   |   
;; ( 6 ) 

;; Step 2
;; (            Gobal Env            )
;;         ^   
;;         |  
;;       ( 6 )
;;       ( 5 )

;; Step 3 
;; (            Gobal Env            )
;;               ^   
;;               |  
;;             ( 6 )
;;             ( 5 )
;;             ( 4 )

;; Step 4
;; (            Gobal Env            )
;;                     ^   
;;                     |  
;;                   ( 6 )
;;                   ( 5 )
;;                   ( 4 )
;;                   ( 3 )

;; Step 5
;; (            Gobal Env            )
;;                           ^   
;;                           |  
;;                         ( 6 )
;;                         ( 5 )
;;                         ( 4 )
;;                         ( 3 )
;;                         ( 2 )

;; Step 6
;; (            Gobal Env            )
;;                                 ^   
;;                                 |  
;;                               ( 6 )
;;                               ( 5 )
;;                               ( 4 )
;;                               ( 3 )
;;                               ( 2 )
;;                               ( 1 )


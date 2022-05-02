#lang sicp

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (* (expmod base (/ exp 2) m)
                       (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))


(expmod 5 32 32)
(remainder (* (expmod 5 16 32)
              (expmod 5 16 32))
           32)

(remainder (*
            (remainder (* (expmod 5 8 32)
                          (expmod 5 8 32)))
            (remainder (* (expmod 5 8 32)
                          (expmod 5 8 32))))
           32)
  

(remainder (*
            (remainder (*
                        (remainder (* (expmod 5 4 32)
                                      (expmod 5 4 32)))
                        (remainder (* (expmod 5 4 32)
                                      (expmod 5 4 32)))))
            (remainder (*
                        (remainder (* (expmod 5 4 32)
                                      (expmod 5 4 32)))
                        (remainder (* (expmod 5 4 32)
                                      (expmod 5 4 32))))))
           32)

(remainder (*
            (remainder (*
                        (remainder (*
                                    (remainder (* (expmod 5 2 32)
                                                  (expmod 5 2 32)))
                                    (remainder (* (expmod 5 2 32)
                                                  (expmod 5 2 32)))))
                        (remainder (*
                                    (remainder (* (expmod 5 2 32)
                                                  (expmod 5 2 32)))
                                    (remainder (* (expmod 5 2 32)
                                                  (expmod 5 2 32)))))))
            (remainder (*
                        (remainder (*
                                    (remainder (* (expmod 5 2 32)
                                                  (expmod 5 2 32)))
                                    (remainder (* (expmod 5 2 32)
                                                  (expmod 5 2 32)))))
                        (remainder (*
                                    (remainder (* (expmod 5 2 32)
                                                  (expmod 5 2 32)))
                                    (remainder (* (expmod 5 2 32)
                                                  (expmod 5 2 32))))))))
           32)

(remainder (*
            (remainder (*
                        (remainder (*
                                    (remainder (* (remainder (* (remainder (* 5 1) 32) (remainder (* 5 1) 32)))
                                                  (remainder (* (remainder (* 5 1) 32) (remainder (* 5 1) 32)))))
                                    (remainder (* (remainder (* (remainder (* 5 1) 32) (remainder (* 5 1) 32)))
                                                  (remainder (* (remainder (* 5 1) 32) (remainder (* 5 1) 32)))))))
                        (remainder (*
                                    (remainder (* (remainder (* (remainder (* 5 1) 32) (remainder (* 5 1) 32)))
                                                  (remainder (* (remainder (* 5 1) 32) (remainder (* 5 1) 32)))))
                                    (remainder (* (remainder (* (remainder (* 5 1) 32) (remainder (* 5 1) 32)))
                                                  (remainder (* (remainder (* 5 1) 32) (remainder (* 5 1) 32)))))))))
            (remainder (*
                        (remainder (*
                                    (remainder (* (remainder (* (remainder (* 5 1) 32) (remainder (* 5 1) 32)))
                                                  (remainder (* (remainder (* 5 1) 32) (remainder (* 5 1) 32)))))
                                    (remainder (* (remainder (* (remainder (* 5 1) 32) (remainder (* 5 1) 32)))
                                                  (remainder (* (remainder (* 5 1) 32) (remainder (* 5 1) 32)))))))
                        (remainder (*
                                    (remainder (* (remainder (* (remainder (* 5 1) 32) (remainder (* 5 1) 32)))
                                                  (remainder (* (remainder (* 5 1) 32) (remainder (* 5 1) 32)))))
                                    (remainder (* (remainder (* (remainder (* 5 1) 32) (remainder (* 5 1) 32)))
                                                  (remainder (* (remainder (* 5 1) 32) (remainder (* 5 1) 32))))))))))
           32)


;; remainder appears 63 times
(remainder (*
            (remainder (*
                        (remainder (*
                                    (remainder (* (remainder (* (remainder (* 5 1) 32) (remainder (* 5 1) 32)))
                                                  (remainder (* (remainder (* 5 1) 32) (remainder (* 5 1) 32)))))
                                    (remainder (* (remainder (* (remainder (* 5 1) 32) (remainder (* 5 1) 32)))
                                                  (remainder (* (remainder (* 5 1) 32) (remainder (* 5 1) 32)))))))
                        (remainder (*
                                    (remainder (* (remainder (* (remainder (* 5 1) 32) (remainder (* 5 1) 32)))
                                                  (remainder (* (remainder (* 5 1) 32) (remainder (* 5 1) 32)))))
                                    (remainder (* (remainder (* (remainder (* 5 1) 32) (remainder (* 5 1) 32)))
                                                  (remainder (* (remainder (* 5 1) 32) (remainder (* 5 1) 32)))))))))
            (remainder (*
                        (remainder (*
                                    (remainder (* (remainder (* (remainder (* 5 1) 32) (remainder (* 5 1) 32)))
                                                  (remainder (* (remainder (* 5 1) 32) (remainder (* 5 1) 32)))))
                                    (remainder (* (remainder (* (remainder (* 5 1) 32) (remainder (* 5 1) 32)))
                                                  (remainder (* (remainder (* 5 1) 32) (remainder (* 5 1) 32)))))))
                        (remainder (*
                                    (remainder (* (remainder (* (remainder (* 5 1) 32) (remainder (* 5 1) 32)))
                                                  (remainder (* (remainder (* 5 1) 32) (remainder (* 5 1) 32)))))
                                    (remainder (* (remainder (* (remainder (* 5 1) 32) (remainder (* 5 1) 32)))
                                                  (remainder (* (remainder (* 5 1) 32) (remainder (* 5 1) 32))))))))))
           32)

;; exp가 짝수 일 때 프로시져가 square와 달리 두배씩 늘어나기 때문에 square를 사용해 줄인 의미가 없어져 Θ(n) 함수가 된다.

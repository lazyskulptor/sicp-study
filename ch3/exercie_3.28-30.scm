;; Exercise 3.28-30

;; GIVEN
(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (lambda ()
        (set-signal! ouput new-value))))
  (add-action! input invert-input)
  'ok)

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-vaue
           (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

;; 3.28
(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-vaue
           (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)


;; 3.29
(define (or-gate a1 a2 output)
  (let ((a3 (make-wire))
        (a4 (make-wire))
        (a5 (make-wire)))
    (inverter a1 a3)
    (inverter a2 a4)
    (and-gate a3 a4 a5)
    (inverter a5 output))
  'ok)

;; a1 ->(inverter-delay)-> a3 \ 
;;                             ==--> (and-delay) -> a5 -> (inverter-delay) -> output
;; a2 ->(inverter-delay)-> a4 /
;; or-dealy = 2 * inverter-delay + 1 * and-delay


;; 3.30
;; GIVEN

(define (half-adder a b s c)   ;; (inverter-delay + 2*and-delay) OR  (and-delay + or-delay)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gat a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out) ;; 2*half + or
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

(define (repple-carry-adder a, b, s, c) 
  (define (iter ak bk c-in sk c-out)
    (when (not (null? ak))
      (full-adder (car ak) (car bk) c-in (car sk) c-out)
      (iter (cdr ak) (cdr bk) c-out (cdr sk) (make-wire))))
  (iter a b c s (make-wire)))

;; n size list operate full-adder n times
;; n * (2 * half-ader + and-delay)
;; and-delay and or-dealy is same
;; n * (2*and-delay + 3*or-delay) <= repple-carry-adder-delay <= n * (4 * and-delay + or-delay + inverter-dealy)


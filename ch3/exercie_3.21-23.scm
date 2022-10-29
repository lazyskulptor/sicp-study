;; Exercise 3.21-23

;; GIVEN
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))
(define (empty-queue? queue) (null? (front-ptr queue)))
(define (make-queue) (cons '() '()))
(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))
(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))
(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue)))


;; 3.21
(define q1 (make-queue))
;; Explain What Eva lu is talking about.
;; Standard Lisp Printer shows only what q1 is pointing.
;; So that Printer show only what (car q1), (cdr q1) are pointing.

;; Why Ben's examples produce the printed results that they do.
;; q1 -> [/][/]
(insert-queue! q1 'a)
;; q1 -> [*][*]  (car q1),(cdr q1) pointing the same.
;;        | /
;;        |/
;;        v
;;       [a][/]
(insert-queue! q1 'b)
;; q1 -> [*][*]     (car q1) hasn't changed. 
;;        |    \     But (cdr q1) changed to point (cons b '())
;;        |     \    And pair pointed by (car q1) start to point other pair.
;;        v      v
;;       [a][*]->[b][/]
(delete-queue! q1)  
;; q1 -> [*][*] (car q1) has changed to point to the same with (cdr q1)
;;         \ |
;;          \|
;;           v
;;   [a][*]->[b][/]
(delete-queue! q1)
;; q1 -> [/][*]  (car q1) has changed to point to null. 
;;           |    Because there was no pair pointed by previous pair.
;;           |
;;           v
;;  [a][*]->[b][/]

(define (print-queue queue)
  (define (worker p)
    (when (not (null? p))
        (display " ")
        (display (car p))
        (worker (cdr p))))
  (display "(")
  (when (not (null? (front-ptr queue)))
    (display (front-queue queue))
    (worker (cdr (front-ptr queue))))
  (display ")")
  (newline))

;; 3.22
(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (set-front-ptr! item) (set! front-ptr item))
    (define (set-rear-ptr! item) (set! rear-ptr item))
    (define (empty-queue?) (null? front-ptr))
    (define (dispatch m)
      (cond
       ((eq? m 'empty-queue?) empty-queue?)
       ((eq? m 'front-queue) front-ptr)
       ((eq? m 'insert-queue!) (lambda (item) (let ((new-pair (cons item '())))
                                                (cond ((empty-queue?)
                                                       (set-front-ptr! new-pair)
                                                       (set-rear-ptr! new-pair)
                                                       (cons front-ptr rear-ptr))
                                                      (else
                                                       (set-cdr! rear-ptr new-pair)
                                                       (set-rear-ptr! new-pair)
                                                       (cons front-ptr rear-ptr))))))
       ((eq? m 'delete-queue!) (lambda ()
                                 (cond ((empty-queue?)
                                        (error "DELETE! called with an empty queue" (cons front-ptr rear-ptr)))
                                       (else
                                        (set-front-ptr! (cdr (front-ptr queue)))
                                        (cons front-ptr rear-ptr)))))))
       (else (error "Undefined operation -- Queue" (cons front-ptr rear-ptr)))
    dispatch))

;; 3.23
(define (front-ptr deque) (car deque))
(define (rear-ptr deque) (cdr deque))
(define (set-front-ptr! deque item) (set-car! deque item))
(define (set-rear-ptr! deque item) (set-cdr! deque item))
(define (empty-deque? deque) (null? (front-ptr deque)))
(define (make-deque) (cons '() '()))

(define (front-deque deque)
  (if (empty-deque? deque)
      (error "FRONT called with an empty queue" deque)
      (car (front-ptr deque))))
(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "FRONT called with an empty queue" deque)
      (car (rear-ptr deque))))

(define (front-insert-deque! deque item)
  (let ((new-pair (cons item (cons '() '()))))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-pair)
           (set-rear-ptr! deque new-pair)
           deque)
          (else
           (let ((front (cdr (front-ptr deque)))
                 (set-car! front new-pair)
                 (set-cdr! (cdr new-pair) front)))
           (set-front-ptr! deque new-pair)
           deque))))
(define (rear-insert-deque! deque item)
  (let ((new-pair (cons item (cons '() '()))))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-pair)
           (set-rear-ptr! deque new-pair)
           deque)
          (else
           (let ((rear (cdr (rear-ptr deque)))
                 (set-cdr! rear new-pair)
                 (set-car! (cdr new-pair) rear)))
           (set-rear-ptr! deque new-pair)
           deque))))

(define (front-delete-queue! deque)
  (cond ((empty-deque? deque)
         (error "DELETE! called with an empty deque" deque))
        (else
         (let ((front (front-ptr deque))
               (second (cddr (front-ptr deque))))
           (cond ((null? second)
                  (set-front-ptr! deque '())
                  (set-rear-ptr! deque '()))
                 (else 
                  (set-front-ptr! deque second)
                  (set-car! (cdr second) '())))
           deque))))
(define (rear-delete-queue! deque)
  (cond ((empty-deque? deque)
         (error "DELETE! called with an empty deque" deque))
        (else
         (let ((first (rear-ptr deque))
               (second (cddr (rear-ptr deque))))
           (cond ((null? second)
                  (set-front-ptr! deque '())
                  (set-rear-ptr! deque '()))
                 (else 
                  (set-rear-ptr! deque second)
                  (set-cdr! (cdr second) '())))
           deque))))

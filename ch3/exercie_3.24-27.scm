;; Exercise 3.24-27

;; GIVEN

(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        false)))

(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value) (cdr table)))))
  'ok)

(define (make-table)
  (list '*table*))

(define (lookup key-1 key-2 table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (cdr record)
              false))
        false)))

(define (insert! key-1 key-2 value table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons key-2 value)
                              (cdr subtable)))))
        (set-cdr! table
                  (cons (liet key-1
                              (cons key-2 value))
                        (cdr table)))))
  'ok)

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                            (cdr local-table)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
        'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

;; 3.24
(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (assoc key records)
      (cond ((null? records) false)
            ((same-key? key (caar records)) (car records))
            (else (assoc key (cdr records)))))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                            (cdr local-table)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
        'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

;; 3.25
(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup keys)
      (define (iter table keys)
        (if (null? keys)
            false
            (let ((subtable (assoc (car keys) (cdr table))))
              (cond ((not (subtable)) false)
                    ((null? (cdr keys)) (cdr subtable))
                    (else (iter (cdr keys) subtable))))))
      (iter local-table key-1 keys))
    (define (insert! keys value)
      (define (iter table keys)
        (let ((subtable (assoc (car keys) (cdr table))))
          (cond ((not (subtable))
                 (set-cdr! table
                           (cons (cons key value) (cdr table))))
                ((null? (cdr keys))
                 (set-cdr! subtable value))
                (else (iter subtable (cdr keys)))))
        'ok)
      (iter local-table keys))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

;; 3.26

;; table      [f|38]
;;   |         ^
;;   v         |
;; [*table*]->[*|*|*]
;;              /   \
;; [b|38]<--|  v    |v-->[m|38]
;;         [*|*|/] [*|*|*]
;;            |       |  \__________
;; [a|99]<--| v [h|1] |      [A|71] \
;;         [*|*|/]  ^ v            ^ v
;;                 [*|*|*]        [*|*|*]
;;  ...

;; Let's say that there is function adjoin-set to insert record
(define (assoc key records)
  (cond ((null? records) false)
        ((element-of-set? ) (car records))
        (else (assoc key (cdr records)))))

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup keys)
      (let ((record (assoc key (cdr local-table))))
        (if record
            (cdr record)
            false)))
    (define (insert! key value)
      (let ((record (assoc key (cdr local-table))))
        (if record
            (set-cdr! record value) ;; TODO insert new record
            (set-cdr! (adjoin-set (cons key value) local-table))))) ;; TODO replace value
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))


;; 3.27
;; GIVEN
(define (fib n)
  (cond ((= 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define memo-fib
  (memoize (lambda (n)
             (cond ((= 0) 0)
                   ((= n 1) 1)
                   (else (+ (memo-fib (- n 1))
                            (memo-fib (- n 2))))))))

(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previously-computed-result (lookup x table)))
        (or previously-computed-result
            (let ((result (f x)))
              (insert! x result table)
              result))))))

;;
;; [ global env, memo-fib, memoize]
;;  memoize:
;;   args: f
;;   args: ... return lambda

;;  memo-fib -> [E1| table, f: (lambda (n) (cond ((= n 0) 0) ((= n 1) 1) (else ...
;;   args: x
;;   body: (lambda (x)
;;           (let ... return value from table or clculaged value of (f x)  


;; [    Global Env                 ]
;; [    memo-fib, memoize          ]
;;      |
;;      v        ^
;;     [*|*]-> [E1|f: (lambda (n) ....)] 

;; table has no value
;; [    Global Env , memo-fib             ]
;;                   ^       \__         ^
;;                   |          \       [E3| n: 3]
;;                   |           v       ^         
;;  (memo-fib 3) -> [E2| x:3]    [*|*]->[E1, f, table]
;;  ** (f 2), (f 1) both are called, because (lookup 3 table) is false.
;;  ** result{1} of (f 1) is saved at table
;;  ** (f 2) calls (memo-fib 1) (memo-fib 0)


;; table has value 1
;; [    Global Env , memo-fib             ]
;;                   ^       \__         ^
;;                   |          \       [E5| n: 2]
;;                   |           v       ^         
;;  (memo-fib 2) -> [E4| x:2]    [*|*]->[E1, f] 
;;  ** (memo-fib 1) get value from table 
;;  ** (memo-fib 0) calls (f 0) and save it's value to table.

;; (memo-fib 3) calls (memo-fib 2) (memo-fib 1) (memo-fib 0) each time.

;; Because time delay in insert and lookup, nth proportion is not guranteed
;; But if value is saved at time exact time when function is called next, proportion is nth.
;; https://stackoverflow.com/questions/52897830/memoization-performance-sicp-exercise-3-27-seems-to-be-wrong#

;; (memoize fib) doesn't work with memoize, because internal function is not wrapped with memoize

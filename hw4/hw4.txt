
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
  (if (> low high)
    null
    (cons low (sequence (+ low stride) high stride))))

(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (let ([index (remainder n (length xs))])
              (list-ref xs index))]))
              
(define (stream-for-n-steps stream n)
  (if (= n 0)
      null
      (letrec ([stream-result (stream)])
        (cons (car stream-result) (stream-for-n-steps (cdr stream-result) (- n 1))))))

(define funny-number-stream
  (letrec ([f (lambda (x) (cons 
                           (if (= (remainder x 5) 0)
                               (- 0 x)
                               x)
                           (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

(define dan-then-dog
  (letrec ([getDan (lambda () (cons "dan.jpg" getDog))]
           [getDog (lambda () (cons "dog.jpg" getDan))])
    (lambda () (getDan))))

(define (stream-add-zero stream)
  (letrec ([f (lambda (st) 
                (letrec ([st-result (st)])
                  (cons (cons 0 (car st-result)) (lambda () (f (cdr st-result))))))])
    (lambda () (f stream))))

(define (cycle-lists list1 list2)
  (letrec ([f (lambda (index1 index2)
                (cons 
                 (cons (list-nth-mod list1 index1) (list-nth-mod list2 index2)) 
                 (lambda () (f (+ index1 1) (+ index2 1)))))])
    (lambda () (f 0 0))))

(define (vector-assoc v vec)
  (letrec ([len (vector-length vec)]
           [check-nth (lambda (n)
                        (if (< n len)
                            (letrec ([elt (vector-ref vec n)])
                              (if (pair? elt)
                                  (if (equal? (car elt) v)
                                      elt
                                      (check-nth (+ n 1)))
                                  (check-nth (+ n 1))))
                            #f))])
    (check-nth 0)))
                                                 
(define (cached-assoc xs n)
  (letrec ([cache (make-vector n #f)]
           [curCell 0]
           [f (lambda (v)
                (letrec ([cachedResult (vector-assoc v cache)])
                  (if cachedResult
                      cachedResult
                      (letrec ([result (assoc v xs)])
                        (if result
                            (begin
                              (vector-set! cache curCell result)
                              (set! curCell (+ curCell 1))
                              (if (= curCell n)
                                  (set! curCell 0)
                                  #f)
                              result)
                            #f)))))])
    (lambda (x) (f x))))

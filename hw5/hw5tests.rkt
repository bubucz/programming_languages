#lang racket

(require "hw5.rkt")

;a test case that uses problems 1, 2, and 4
; should produce (list (int 10) (int 11) (int 16))
(define test1
  (mupllist->racketlist
   (eval-exp (call (call mupl-mapAddN (int 7))
                   (racketlist->mupllist 
                    (list (int 3) (int 4) (int 9)))))))

(define double (fun #f "value" (add (var "value") (var "value"))))

(define res (eval-exp (call double (int 3))))

;(define res2 (mlet* (list (cons "a" (int 3))) (var "a")))

(define res3 (ifeq (int 2) (int 2) (mprint "e3") (mprint "e4")))

(define res4 (mlet "e1" (int 1) (mlet "e2" (add (int 2) (var "e1")) (add (var "e1") (var "e2")))))

(define res5 (eval-exp (mlet* (list (cons "e1" (int 3)) (cons "e2" (add (var "e1") (int 2)))) (add (var "e1") (var "e2")))))

;(define add2 (eval-exp (call addN (int 2))))
(define add2 (call addN (int 2)))

;(define add2ToList (eval-exp (call mupl-map add2)))
(define add2ToList (call mupl-map add2))

;(define l4 (eval-exp (call add2ToList (apair (int 1) (aunit)))))

(define sum-list (fun "sum-list" "lst"
                      (ifeq (isaunit (var "lst")) (int 1)
                            (int 0)
                            (add (fst (var "lst")) (call (var "sum-list") (snd (var "lst")))))))

(define l2 (list (int 1) (int 2) (int 3) (int 12)))
;(define l2 (cons (int 1) (cons (int 2) (cons (int 3) null))))
(define l3 (racketlist->mupllist l2))
(define s1 (eval-exp (call sum-list l3)))

(define printOnes (fun "print-ones" "count"
                       (ifgreater (var "count") (int 0)
                                  (mbegin (mprint "1") (call (var "print-ones") (add (var "count") (int -1))))
                                  (mprint "0"))))

(define print3
  (mlet "three" (int 3)
        (call printOnes (var "three"))))

(define print3again
  (mlet "printOnes" printOnes
        (call (var "printOnes") (int 5))))

(define r4 (eval-exp (call (call mupl-mapAddN (int 4)) l3)))

(define l5 (apair (int 1) (aunit)))

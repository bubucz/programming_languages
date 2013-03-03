;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

(define debug #f)

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0
(struct mprint (msg) #:transparent)
(struct mbegin (e1 e2) #:transparent)

;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1

;; CHANGE (put your solutions here)
(define (racketlist->mupllist lst)
  (if (null? lst)
      (aunit)
      (apair (car lst) (racketlist->mupllist (cdr lst)))))

(define (mupllist->racketlist lst)
  (if (aunit? lst)
      null
      (cons (apair-e1 lst) (mupllist->racketlist (apair-e2 lst)))))

;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (let ([val (envlookup env (var-string e))])
           (if (fun? val)
               (eval-under-env val env)
               val))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        [(int? e) e]
        [(closure? e) e]
        [(aunit? e) e]
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1) (int v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL comparison of non-number")))]
        [(call? e)
         (letrec ([aArg (eval-under-env (call-actual e) env)]
                  [aClosure (eval-under-env (call-funexp e) env)])
           (if (not (closure? aClosure))
               (begin (print aClosure) (error "MUPL call with not a closure"))
               (letrec ([oldEnv (closure-env aClosure)]
                        [f (closure-fun aClosure)]
                        [argName (fun-formal f)]
                        [funName (fun-nameopt f)]
                        [envWithFunc (if funName 
                                         (cons (cons funName f) oldEnv) 
                                         oldEnv)]
                        [newEnv (cons (cons argName aArg) envWithFunc)]
                        [callResult (eval-under-env (fun-body f) newEnv)])
                 (if (fun? callResult)
                     (eval-under-env callResult newEnv)
                     callResult))))]
        ;[(fun? e) (eval-under-env (call (closure env e)) env)]
        [(fun? e) (closure env e)]
        [(mlet? e)
         (letrec ([v (eval-under-env (mlet-e e) env)]
                  [newEnv (cons (cons (mlet-var e) v) env)])
           (eval-under-env (mlet-body e) newEnv))]
        [(apair? e)
         (letrec ([v1 (eval-under-env (apair-e1 e) env)]
                  [v2 (eval-under-env (apair-e2 e) env)])
           (apair v1 v2))]
        [(fst? e)
         (letrec ([pair (eval-under-env (fst-e e) env)])
           (if (apair? pair)
               (apair-e1 pair)
               (error "MUPL fst with not a pair")))]
        [(snd? e)
         (letrec ([pair (eval-under-env (snd-e e) env)])
           (if (apair? pair)
               (apair-e2 pair)
               (error "MUPL snd with not a pair")))]
        [(isaunit? e)
         (let ([v (eval-under-env (isaunit-e e) env)])
           (if (aunit? v)
               (int 1)
               (int 0)))]
        [(mprint? e) (print (mprint-msg e))]
        [(mbegin? e) (begin 
                       (eval-under-env (mbegin-e1 e) env) 
                       (eval-under-env (mbegin-e2 e) env))]
        ;; CHANGE add more cases here
        [#t (begin (print e) (error "bad MUPL expression"))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3) (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e)
  (if (null? lstlst)
      e
      (let ([valToEnv (car lstlst)])
        (mlet (car valToEnv) (cdr valToEnv) (mlet* (cdr lstlst) e)))))
  
(define (ifeq e1 e2 e3 e4) 
  (ifgreater e1 e2 e4
             (ifgreater e2 e1 e4 e3)))

;; Problem 4

(define mupl-map
  (fun #f "func"
       (fun "map-helper" "lst"
            (ifeq (isaunit(var "lst")) (int 0)
                  (apair 
                   (call (var "func") (fst (var "lst"))) 
                   (call (var "map-helper") (snd (var "lst"))))
                  (aunit)))))
            

(define addN 
  (fun #f "N"
       (fun #f "value"
            (add (var "N") (var "value")))))

(define mupl-mapAddN
  (fun #f "N"
       (call mupl-map (call addN (var "N")))))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))

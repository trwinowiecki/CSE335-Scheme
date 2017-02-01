#lang racket
(#%provide (all-defined))
(#%require (lib "eopl.ss" "eopl"))

#|
IMPORTANT:
Overall, you are allowed to change this file in any way that does *not* affect the
compilation of the accompanying test file. Changes that are almost certain to break
the above restriction are:
  - changing the names of any definitions that are explicitely used in the tests
    (e.g. function names, relevant constants)

If there are any specific instructions for a problem, please read them carefully. Otherwise,
follow these general rules:

   - replace the 'UNIMPLEMENTED symbol with your solution
   - you are NOT allowed to change the number of arguments of the pre-defined functions,
     because changing the number of arguments automatically changes the semantics of the 
     function. Changing the name of the arguments is permitted since that change only
     affects the readability of the function, not the semantics.
   - you may write any number of helper functions

When done, make sure that the accompanying test file compiles. 
If you cannot come up with a correct solution then please make the answer-sheet
compiles. If you have partial solutions that do not compile please comment them out,
if this is the case, the default definitions will have to be present since the tests
will be expecting functions with the names defined here.

Submission guidelines:
   - please rename this file to hw05-yourlastname.rkt prior to submission
   - also rename hw05-tests.rkt to hw05-yourlastname-tests.rkt
   - upload both hw05-yourlastname.rkt and hw05-tests.rkt
|#
;=======================================01======================================
(define (invalid-args-msg fun-name-as-string
                          expected-value-type-as-predicate-string
                          received)
  (string-append "Invalid arguments in: " fun-name-as-string " --- "
                 "expected: " expected-value-type-as-predicate-string " --- "
                 "received: " (~a received)
                 )
)

;You can compare the contents of this answer sheet with the answer sheet of the
;previous homework to infer what is generated automatically by define-datatype.

(define-datatype step step?
  (up-step (n number?))
  (down-step (n number?))
  (left-step (n number?))
  (right-step (n number?))
  (seq-step (st-1 step?) (st-2 step?))
)

(define (up-step? st)
  (and
   (step? st)
   (cases step st
    (up-step (num) #t)
    (else #f)
   )
  )
)

(define (down-step? st)
  (and
   (step? st)
   (cases step st
    (down-step (num) #t)
    (else #f)
   )
  )
)

(define (left-step? st)
  (and
   (step? st)
   (cases step st
    (left-step (num) #t)
    (else #f)
   )
  )
)

(define (right-step? st)
  (and
   (step? st)
   (cases step st
    (right-step (num) #t)
    (else #f)
   )
  )
)

(define (seq-step? st)
  (and (step? st)
       (and (step? (seq-step->st-1 st))
            (step? (seq-step->st-2 st))))
)

;;to avoid needless duplication we will only implement one extractor to handle all the
;;simple steps, rather than 4. So this should take: up, down, left and right steps.
(define (single-step->n st)
  (if (step? st)
      (cases step st
        (up-step (num) num)
        (down-step (num) num)
        (left-step (num) num)
        (right-step (num) num)
        (else (invalid-args-msg "single-step->n" "single-step?" "not-a-single-step"))
      )
      (raise (invalid-args-msg "single-step->n" "single-step?" "not-a-single-step")))
)

;;two extractors, one for each piece of data representing a sequential step
(define (seq-step->st-1 st)
  (if (step? st)
      (cases step st
        (seq-step (st-1 st-2) st-1)
        (else (invalid-args-msg "seq-step->st-1" "seq-step?" "not-a-seq-step"))
      )
      (raise (invalid-args-msg "seq-step->st-1" "seq-step?" "not-a-seq-step")))
)


(define (seq-step->st-2 st)
  (if (step? st)
      (cases step st
        (seq-step (st-1 st-2) st-2)
        (else (invalid-args-msg "seq-step->st-2" "seq-step?" "not-a-seq-step"))
      )
      (raise (invalid-args-msg "seq-step->st-2" "seq-step?" "not-a-seq-step")))
)
;;===================================
(define (move start-p step)
  (let
      ([x (car start-p)]
       [y (cadr start-p)])
    (cond
      ((up-step? step) (list x (+ y (single-step->n step))))
      ((down-step? step) (list x (- y (single-step->n step))))
      ((left-step? step) (list (- x (single-step->n step)) y))
      ((right-step? step) (list (+ x (single-step->n step)) y))
      ((seq-step? step) (move (move start-p (seq-step->st-1 step)) (seq-step->st-2 step)))
    )
  )
)
;=======================================02======================================
;2.a
(define (exception-no-binding-msg sym)
  (string-append "No binding for '" (~a sym))
)

;
(define-datatype environment environment?
  ;delete this unimplemented variant from your solution
  (empty-env)
  (extend-env (var symbol?) (val number?) (env environment?))
  (extend-env-final (sym symbol?) (val number?) (env environment?))
)

(define (apply-env env search-sym)
  (cases environment env
    (empty-env () (raise "No such variable found"))
    (extend-env (var val saved-env)
                (if (eqv? search-sym var)
                    val
                    (apply-env saved-env search-sym)))
    (extend-env-final (var val saved-env)
                      (if (eqv? search-sym var)
                          val
                          (apply-env saved-env search-sym)))
  )
)

;==========
;2.b
(define (exception-sym-final-msg sym)
  (string-append "Symbol '" (~a sym) " is final and cannot be overriden.")
)

;It is prefered to give meaningfull names to marker values.
;In the tests we will be using these two values to invoke
;the extend-env-wrapper function
(define FINAL #t)
(define NON-FINAL #f)

(define (extend-env-wrapper sym val old-env final?)
  (define (is-var-final? env)
    (cases environment env
      (empty-env () #f)
      (extend-env (sym val prev-env) (is-var-final? prev-env))
      (extend-env-final (store-sym val prev-env)
                        (if (equal? sym store-sym)
                            #t
                            (is-var-final? prev-env)))
    )
  )

  (if (is-var-final? old-env)
      (exception-sym-final-msg sym)
      (if final?
          (extend-env-final sym val old-env)
          (extend-env sym val old-env)))
)
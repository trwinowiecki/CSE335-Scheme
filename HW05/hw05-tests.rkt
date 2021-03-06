#lang racket
(#%provide (all-defined))
(#%require rackunit)

(#%require "hw05-winowiecki.rkt")

;this function will run all the tests.
(define (test-all)
  (test p1-a)
  (test p1-b)
  (test p2-a)
  (test p2-b)
  )

; Here, in order to make this file compile, we have put an impossible assertion: (check-true #f)
; at the beginning of each test-suite, and have commented out the test cases. You need to delete
; this impossible assertion for each test-suite, and uncomment the test cases. We repeat this 
; instruction at the beginning of each test-suite below, please follow it well.

;===============================================================================
;======================================01=======================================
;===============================================================================
(define p1-a
  (test-suite
   "1.a - steps, define-datatype"
   
   ;now that I have your attention, delete this impossible assertion (check-true #f) below. 
   ;Then uncomment the test cases for the environment after you've written the define-datatype for it.
   
   (test-case
    "test 1, up-step"
    (check-true (up-step? (up-step 3)))
    
    (check-false (up-step? "not a step"))
    )
   
   
   (test-case
    "test 2, down-step"
    (check-true (down-step? (down-step 3)))
    
    (check-false (down-step? "not a step"))
    
    )
   
   (test-case
    "test 3, left-step"
    (check-true (left-step? (left-step 3)))
    
    (check-false (left-step? "not a step"))
    )
   
   
   (test-case
    "test 4, right-step"
    (check-true (right-step? (right-step 3)))
    
    (check-false (right-step? "not a step"))
    )
   
   (test-case
    "test 5, seq-step"
    (check-true
     (seq-step? (seq-step (right-step 3) (up-step 4)))) 
    
    (check-false (seq-step? "not a step"))
    )
   
   (test-case
    "test 6, step? predicate"
    
    (check-true (step? (up-step 3)))
    (check-true (step? (down-step 3)))
    (check-true (step? (left-step 3)))
    (check-true (step? (right-step 3)))
    (check-true (step? (seq-step (right-step 3) (up-step 4))))
    
    (check-false (step? "not a step"))
    )
   
   (test-case
    "test 7, single->step extractor"
    (check-equal?
     (single-step->n (up-step 3))
     3)
    
    (check-equal?
     (single-step->n (down-step 3))
     3)
    
    (check-equal?
     (single-step->n (left-step 3))
     3)
    
    (check-equal?
     (single-step->n (right-step 3))
     3)
    
    (335-check-exn
     (single-step->n "not-a-single-step")
     "Invalid arguments in: single-step->n --- expected: single-step? --- received: not-a-single-step")
    
    (check-equal?
     (single-step->n (seq-step->st-1 (seq-step (left-step 3) (right-step 4))))
     3)
    
    (check-equal?
     (single-step->n (seq-step->st-2 (seq-step (left-step 3) (right-step 4))))
     4)
    
    )
   );end test-suite
  )

;;======
(define p1-b
  (test-suite
   "1.b - steps, move"
   
   ;now that I have your attention, delete this impossible assertion (check-true #f) 
   ;below. Then uncomment the test cases for the environment after you've written 
   ;the define-datatype for it.
   
   (test-case
    "move up"
    (check-equal?
     (move '(0 0) (up-step 3))
     '(0 3)
     ))
   
   (test-case
    "move-down"
    (check-equal?
     (move '(0 0) (down-step 3))
     '(0 -3)
     )
    )
   
   (test-case
    "move left"
    (check-equal?
     (move '(0 0) (left-step 3))
     '(-3 0)
     )
    )
   
   (test-case
    "move-right"
    (check-equal?
     (move '(0 0) (right-step 3))
     '(3 0)
     )
    )
   
   (test-case
    "move in sequence: up, right"
    (check-equal?
     (move '(0 0) (seq-step (up-step 3)(right-step 3)))
     '(3 3)
     )
    )
   
   (test-case
    "move in sequence: up, down; they should cancel each other" 
    (check-equal?
     (move '(0 0) (seq-step (up-step 3)(down-step 3)))
     '(0 0)
     )
    )
   
   (test-case
    "move in sequence of sequence: up, left, right; they should cancel each other"
    
    (check-equal?
     (move '(0 0) (seq-step (up-step 10) (seq-step (left-step 7) (right-step 4))))
     '(-3 10)
     )
    )
   )
  )

;===============================================================================
;======================================02=======================================
;===============================================================================
(define p2-a
  (test-suite
   "2.a - environment, define datatype"
   
   ;now that I have your attention, delete this impossible assertion (check-true #f)
   ;below. Then uncomment the test cases for the environment after you've written 
   ;the define-datatype for it.
   
   (test-case
    "empty-environment"
    (environment? (empty-env))
    )
   
   (test-case
    "extend-env"
    (environment? (extend-env 'x 24 (empty-env)))
    (environment? (extend-env 'x 42 (extend-env 'y 42 (empty-env))))
    )
   
   (test-case
    "apply-env"
    (define x-24 (extend-env 'x 24 (empty-env)))
    (define x-42$y-24 (extend-env 'x 42 (extend-env 'y 24 (empty-env))))
    
    (check-equal? (apply-env x-24 'x) 24)
    
    (check-equal? (apply-env x-42$y-24 'x) 42)
    (check-equal? (apply-env x-42$y-24 'y) 24)
    
    (define shadow-x (extend-env 'x 99 x-42$y-24))
    (check-equal? (apply-env shadow-x 'x) 99 "x, should be 99 because its previous definition was shadowed")
    
    (check-equal? (apply-env x-42$y-24 'x) 42
                  "the previous binding of 'x should still be in the old environment.
                   Extend environment has no side effects.")
    
    
    )
   )
  )

;;========
(define p2-b
  (test-suite
   "2.b extend-env-wrapper"
   
   ;now that I have your attention, delete this impossible assertion (check-true #f) 
   ;below. Then uncomment the test cases for the environment after you've written the 
   ;define-datatype for it.
   
   (test-case
    "extend-env-wrapper is just another constructor, except that its a bit smarter"
    (check-true (environment? (extend-env-wrapper 'x 42 (empty-env) NON-FINAL)))
    (check-true (environment? (extend-env-wrapper 'x 42 (empty-env) FINAL)))
    )
   
   (test-case
    "extend-env-wrapper, single mapping env"
    (define x-42-nf (extend-env-wrapper 'x 42 (empty-env) NON-FINAL))
    (define x-42-f (extend-env-wrapper 'x 42 (empty-env) FINAL))
    
    ;apply-env should work just as before
    (check-equal? (apply-env x-42-nf 'x) 42)
    (check-equal? (apply-env x-42-f 'x) 42)
    
    (check-equal? (apply-env (extend-env-wrapper 'x 99 x-42-nf NON-FINAL) 'x)
                  99
                  "trying to override x when it is *not* final, should work regardless of extend type")
    
    (check-equal? (apply-env (extend-env-wrapper 'x 99 x-42-nf FINAL) 'x)
                  99
                  "trying to override x when it is *not* final, should work regardless of extend type")
    
    (335-check-exn (extend-env-wrapper 'x 99 x-42-f NON-FINAL)
                   "Symbol 'x is final and cannot be overriden."
                   )
    
    (335-check-exn (extend-env-wrapper 'x 99 x-42-f FINAL)
                   "Symbol 'x is final and cannot be overriden."
                   )
    )
   
   (test-case
    "extend-env-wrapper with more than one binding in env."
    (define x-42-nf (extend-env-wrapper 'x 42 (empty-env) NON-FINAL))
    (define x-42-f (extend-env-wrapper 'x 42 (empty-env) FINAL))
    
    (define y-24-nf$x-42-nf (extend-env-wrapper 'y 24 x-42-nf NON-FINAL))
    (define y-24-nf$x-42-f (extend-env-wrapper 'y 24 x-42-f NON-FINAL))
    
    (check-equal? (apply-env y-24-nf$x-42-nf 'y) 24)
    (check-equal? (apply-env y-24-nf$x-42-f 'y) 24)
    
    (check-equal? (apply-env (extend-env-wrapper 'y 99 y-24-nf$x-42-nf FINAL) 'y)
                  99
                  "you can shadow with final extensions, y-24-nf$x-42-nf")
    
    (check-equal? (apply-env (extend-env-wrapper 'y 99 y-24-nf$x-42-f FINAL) 'y)
                  99
                  "you can shadow with final extensions, y-24-nf$x-42-f")
    
    (335-check-exn (extend-env-wrapper 'x 99 y-24-nf$x-42-f NON-FINAL)
                   "Symbol 'x is final and cannot be overriden."
                   )
    )
   );end test-suite
  )


;===============================================================================
;============================test infrastructure================================
;===============================================================================
(require rackunit/text-ui)

(define (test suite)
  (run-tests suite 'verbose)  
  )

(define-syntax 335-check-exn
  (syntax-rules ()
    [ (335-check-exn expression exn-msg)
      (check-equal? 
       (with-handlers ([string? (lambda (err-msg) err-msg)]) 
         expression)
       exn-msg)
      ]
    )
  )
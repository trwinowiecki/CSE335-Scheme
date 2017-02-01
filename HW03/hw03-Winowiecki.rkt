#lang racket
(#%provide (all-defined))

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
   - you may write any number of helper functions as you want.

When done, make sure that the accompanying test file compiles. 
|#
;======================================01=======================================
(define (foldl-335 op zero-el lst)
  (if (null? lst)
      zero-el
      (foldl-335 op (op (car lst) zero-el) (cdr lst)))
)

;---
(define (foldr-335 op zero-el lst)
   (if (null? lst)
      zero-el
      (foldr-335 op (op (last lst) zero-el) (reverse (cdr (reverse lst)))))
)

;======================================02=======================================
(define (andmap-335 test-op lst)
  (foldl-335 (lambda (x y) (and (test-op x) y)) #t lst)
)

;======================================03=======================================
(define (filter-335 test-op lst)
  (foldl-335 (lambda (x y) (if (test-op x) (append y (list x)) y)) (list) lst)
)


;======================================04=======================================
(define (map-reduce m-op r-op zero-el lst)
  (foldl r-op zero-el (map m-op lst))
)

;======================================05=======================================
(define (series n)
  (foldl + 0 (map (lambda (x) (/ (power x) (fact (+ 1 x) 1))) (range (+ 1 n))))
)

(define (power n)
  (if (odd? n)
      -1
      1)
)

(define (fact n hold)
  (if (= n 0)
      hold
      (fact (- n 1) (* hold n)))
)

;======================================06=======================================
(define (zip lst1 lst2)
  (map (lambda (x y) (list x y)) lst1 lst2)
)

;======================================07=======================================
(define (matrix-to-vector op mat)
  (apply map op mat)
)


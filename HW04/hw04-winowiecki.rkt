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
   - you may write any number of helper functions

When done, make sure that the accompanying test file compiles. 
If you cannot come up with a correct solution then please make the answer-sheet
compiles. If you have partial solutions that do not compile please comment them out,
if this is the case, the default definitions will have to be present since the tests
will be expecting functions with the names defined here.

Submission guidelines:
   - please rename the file to hw04-yourlastname-answer.rkt prior to submission
   - only the renamed file file needs to be uploaded
|#
;======================================01=======================================
(define-syntax-rule (for {var <- value-range} yield result)
  (map (lambda (var) result) value-range)
)

;======================================02=======================================
(define test-var-1 42)
(define-syntax-rule (seq expr1 expr2)
  ((lambda ()
    expr1 expr2))
)

;====
(define-syntax-rule (while condition body)
  ((lambda (a) (a a))
   (lambda (iter)
     (cond
       (condition body (iter iter))
       (else 0))))
)

;======================================03=======================================

#|
<step> ::=  <step>  <step>       "seq-step"
          | "up" number          "up-step"
          | "down" number        "down-step"
          | "left" number        "left-step"
          | "right" number       "right-step"
|#
;example of how to create the error message for the "up-step" constructor
;> (invalid-args-msg "up-step" "number?" '(1 2 3 4))
;where '(1 2 3 4) should be replaced by the actual violating value.

;;you can reorder the functions below if it better suits your needs
(define (up-step n)
  (if (number? n)
      (list 'up-step n)
      "Invalid arguments in: up-step --- expected: number? --- received: not-a-number")
)

(define (down-step n)
  (if (number? n)
      (list 'down-step n)
      "Invalid arguments in: down-step --- expected: number? --- received: not-a-number")
)

(define (left-step n)
  (if (number? n)
      (list 'left-step n)
      "Invalid arguments in: left-step --- expected: number? --- received: not-a-number")
)

(define (right-step n)
  (if (number? n)
      (list 'right-step n)
      "Invalid arguments in: right-step --- expected: number? --- received: not-a-number")
)

(define (seq-step st-1 st-2)
  (if (and (step? st-1) (step? st-2))
      (list 'seq-step st-1 st-2)
      "Invalid arguments in: seq-step --- expected: step? --- received: not-a-step")
)

;;====
(define (up-step? st)
  (if (and (pair? st) (equal? (car st) 'up-step))
      #t
      #f)
)

(define (down-step? st)
  (if (and (pair? st) (equal? (car st) 'down-step))
      #t
      #f)
)

(define (left-step? st)
  (if (and (pair? st) (equal? (car st) 'left-step))
      #t
      #f)
)

(define (right-step? st)
  (if (and (pair? st) (equal? (car st) 'right-step))
      #t
      #f)  
)

(define (seq-step? st)
  (if (and (pair? st) (equal? (car st) 'seq-step) (step? (cadr st)) (step? (caddr st)))
      #t
      #f)
)

;This is a predicate that tells you whether or not something is a step,
;it should return true when given either up, down, left, right or seq steps.
(define (step? st)
  (cond
    ((up-step? st) #t)
    ((down-step? st) #t)
    ((left-step? st) #t)
    ((right-step? st) #t)
    ((seq-step? st) #t)
    (else #f))
)

;; to avoid needless duplication we will only implement one extractor to handle all the
;; simple steps, rather than four of them. 
;; So this should take: up, down, left and right steps.
(define (single-step->n st)
  (if (step? st)
      (cadr st)
      "Invalid arguments in: single-step->n --- expected: single-step? --- received: not-a-single-step")
)

;;two extractors, one for each piece of data representing a sequential step
(define (seq-step->st-1 st)
  (if (seq-step? st)
      (cadr st)
      "Invalid arguments in: (seq-step->st-1 --- expected: seq-step? --- received: not-a-seq-step")
)


(define (seq-step->st-2 st)
  (if (seq-step? st)
      (caddr st)
      "Invalid arguments in: (seq-step->st-2 --- expected: seq-step? --- received: not-a-seq-step")
)

;;===================================
(define (move start-p step)
  (let
      ([n (cadr step)]
       [x (car start-p)]
       [y (cadr start-p)])
    (cond
      ((up-step? step) (seq (set! start-p (list x (+ y n))) start-p))
      ((down-step? step) (seq (set! start-p (list x (- y n))) start-p))
      ((left-step? step) (seq (set! start-p (list (- x n) y)) start-p))
      ((right-step? step) (seq (set! start-p (list (+ x n) y)) start-p))
      ((seq-step? step) (seq (set! start-p (move (move start-p (cadr step)) (caddr step))) start-p))
      (else start-p)))
)

;======================================04=======================================


;singleton-set should return a function that takes a number as an argument and
;tells whether or not that number is in the set
(define (singleton-set x)
  (lambda (val)
    (equal? x val))
)

;the set of all elements that are in either 's1' or 's2'
(define (union s1 s2)
  (lambda (val)
    (or (s1 val) (s2 val)))
)

;the set of all elements that are in both  in 's1' and 's2'
(define (intersection s1 s2)
  (lambda (val)
    (and (s1 val) (s2 val)))
)

;the set of all elements that are in 's1', but that are not in 's2'
(define (diff s1 s2)
  (lambda (val)
    (and (s1 val) (not (s2 val))))
)

;returns the subset of s, for which the predicate 'predicate' is true.
(define (filter predicate s)
  (lambda (val)
    (and (predicate val) (s val)))
)

;we assume that the sets can contain only numbers between 0 and bound
(define bound 100)

;returns whether or not the set contains at least an element for which
;the predicate is true. s below is the parameter standing for a given set
(define (exists? predicate s)
  (ormap
   (lambda (val)
     (and (predicate val) (s val)))
   (range bound))
)

;returns whether or not the predicate is true for all the elements
;of the given set s
(define (all? predicate s)
  (andmap
   (lambda (val)
     (or (not (s val)) (and (predicate val) (s val))))
   (range bound))
)

;returns a new set where "op" has been applied to all elements
; NOTE: just because a procedure/function has the word "map" in it, it 
;       doesn't mean you have to use map higher order function to implement it. 
;       Map is a functional operation with well defined behavior that 
;       is not tied to any implementation.
(define (map-set op s)
  (lambda (val2)
    (ormap
     (lambda (val1)
       (and (s val1) (eq? (op val1) val2)))
     (range bound)))
)

;just a sample predicate
(define (prime? p)
  (define (non-divisible-by n d)
    (cond
     ((= d 1) #t)
     (else (if(= (remainder n d) 0)
          #f
          (non-divisible-by n (- d 1))))))
  (if (= p 1)
      #f
      (non-divisible-by p (- p 1))))

;broke
;(define (prime? n)
;  (define (non-divisible? n)
;    (lambda (i)
;      (not (= (modulo n i) 0))))
;  (define range-of-prime-divisors (cddr (range (+ (integer-sqrt n) 1))))
;  (if (or (equal? n 0) (equal? n 1))
;      #f
;      (andmap (non-divisible? n) range-of-prime-divisors)
;      )
;)

;=====================================05====================================
; FYI:
;  to emphasize the procedural-based approach to implement "step" data type and to
;  contrast it with the data structure-based approach for "step" implementation 
;  used in p3, here we add "-proc" suffix to each corresponding function name.

;====p5-a================
(define (up-step-proc n)
  (if (number? n)
      (lambda (e)
        (cond
          ((eq? e 'size) n)
          ((eq? e 'up) #t)
          (else #f)))
      "Invalid arguments in: up-step-proc --- expected: number? --- received: not-a-number")
)

(define (down-step-proc n)
  (if (number? n)
      (lambda (e)
        (cond
          ((eq? e 'size) n)
          ((eq? e 'down) #t)
          (else #f)))
      "Invalid arguments in: down-step-proc --- expected: number? --- received: not-a-number")
)

(define (left-step-proc n)
  (if (number? n)
      (lambda (e)
        (cond
          ((eq? e 'size) n)
          ((eq? e 'left) #t)
          (else #f)))
      "Invalid arguments in: left-step-proc --- expected: number? --- received: not-a-number")
)

(define (right-step-proc n)
  (if (number? n)
      (lambda (e)
        (cond
          ((eq? e 'size) n)
          ((eq? e 'right) #t)
          (else #f)))
      "Invalid arguments in: right-step-proc --- expected: number? --- received: not-a-number")
)

(define (seq-step-proc st-1 st-2)
  (if (and (step-proc? st-1) (step-proc? st-2))
      (lambda (e)
        (or
         (if (eq? e 'first)
             (lambda (f)
               (st-1 f))
             #f)
         (if (eq? e 'second)
             (lambda (f)
               (st-2 f))
             #f)))
      "Invalid arguments in: seq-step-proc --- expected: step-proc? --- received: not-a-step-proc")
)

;;====
(define (up-step-proc? st)
  (if (not (string? st))
      (st 'up)
      #f)
)

(define (down-step-proc? st)
  (if (not (string? st))
      (st 'down)
      #f)
)

(define (left-step-proc? st)
  (if (not (string? st))
      (st 'left)
      #f)
)

(define (right-step-proc? st)
  (if (not (string? st))
      (st 'right)
      #f)
)

(define (seq-step-proc? st)
  (if (not (string? st))
      (and (step-proc? (st 'first)) (step-proc? (st 'second)))
      #f)
)

;This is a predicate that tells you whether or not st is a step,
; it should return true when given either up, down, left, right or seq steps.
(define (step-proc? st)
  (cond
   ((up-step-proc? st) #t)
   ((down-step-proc? st) #t)
   ((left-step-proc? st) #t)
   ((right-step-proc? st) #t)
   ((seq-step-proc? st) #t)
   (else #f))
)

;;to avoid needless duplication we will only implement one extractor to handle all the
;; simple steps, rather than four of them. So this should take: up, down, left and right 
;; steps. 
(define (single-step-proc->n st)
  (if (step-proc? st)
      (st 'size)
      "Invalid arguments in: single-step-proc->n --- expected: single-step-proc? --- received: not-a-single-step-proc")
)

;;two extractors
(define (seq-step-proc->st-1 st)
  (st 'first)
)


(define (seq-step-proc->st-2 st)
  (st 'second)
)

;;========p5-b
(define (move-proc start-p step-proc)
  (let
      ([n (step-proc 'size)]
       [x (first start-p)]
       [y (second start-p)])
    (cond
      ((up-step-proc? step-proc) (seq (set! start-p (list x (+ y n))) start-p))
      ((down-step-proc? step-proc) (seq (set! start-p (list x (- y n))) start-p))
      ((left-step-proc? step-proc) (seq (set! start-p (list (- x n) y)) start-p))
      ((right-step-proc? step-proc) (seq (set! start-p (list (+ x n) y)) start-p))
      ((seq-step-proc? step-proc) (seq (set! start-p
                                     (move-proc (move-proc start-p (step-proc 'first)) (step-proc 'second))) start-p))
      (else start-p)))
)
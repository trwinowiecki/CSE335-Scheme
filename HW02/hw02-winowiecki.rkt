#lang racket
(#%provide (all-defined))

#|
If there are any specific instructions for a problem, please read them carefully. Otherwise,
follow these general rules:
   - replace the 'UNIMPLEMENTED symbol with your solution
   - you are NOT allowed to change the names of any definition
   - you are NOT allowed to change the number of arguments of the pre-defined functions,
     but you can change the names of the arguments if you deem it necessary.
   - make sure that you submit an asnwer sheet that compiles! If you cannot write
     a correct solution at least make it compile, if you cannot make it compile then
     comment it out. In the latter case, make sure that the default definitions
     for the problem are still present. 
   - you may use any number of helper functions you deem necessary.

When done, make sure that you do not get any errors when you hit the "Run" button. 
If you cannot come up with a correct solution then please make the answer-sheet
compile correctly and comment any partial solution you might have; if this is the case,
the default definitions will have to be present!
|#
;======================================01=======================================
(define (list-of-even-numbers? lst)
  (if (null? lst)
      #t
      (if (number? (car lst))
          (if (even? (car lst))
              (list-of-even-numbers? (cdr lst))
              #f)
          #f))
)

;======================================02=======================================
;;for n > 0
;Sn = 1/1 + 1/4 + 1/9 + 1/16 + ...
(define (series-a n)
  (if (> n 0)
      (+ (/ 1 (* n n)) (series-a (- n 1)))
      0)
)

;====
;;for n >= 0
;Sn = 1 - 1/2 + 1/6 - 1/24 + ...
(define (series-b n)
  (if (> n 0)
      (if (even? n)
          (+ (series-b (- n 1)) (/ 1 (fact (+ 1 n) 1)))
          (- (series-b (- n 1)) (/ 1 (fact (+ 1 n) 1))))
      1)
)

(define (fact n acc)
  (if (= n 0)
      acc
      (fact (- n 1) (* n acc)))
)

;======================================03=======================================
(define (carpet n)
  (if (= n 0)
    '((%))
    (top-bot (right-sym n) (+ 1 (* 2 n)) (list) (extend-line (right-sym n) (list) (carpet (- n 1)))))
)

(define (right-sym n)
  (if (even? n)
    '%
    '+)
)

(define (extend-line sym hold lst)
  (if (null? lst)
    hold
    (extend-line sym (append hold (list (append (cons sym (car lst)) (list sym)))) (cdr lst)))
)

(define (top-bot sym n hold lst)
  (if (= 0 n)
    (append (list hold) lst (list hold))
    (top-bot sym (- n 1) (append hold (list sym)) lst))
)

;======================================04=======================================
(define (pascal n)
  (if (= n 1)
      '((1))
      (add-ones (list) n (construct (list) (list) n (pascal (- n 1)))))
)

(define (add-ones hold n lst)
  (if (= n 2)
      (append lst '((1 1)))
      (append (reverse (cdr (reverse lst))) (list (append (cons '1 (last lst)) (list 1)))))
)

(define (remove-last n lst)
  (if (= n 2)
      lst
      (reverse (cdr (reverse lst))))
)

(define (construct hold last-line n lst)
  (if (or (= 1 (length last-line)) (< n 3))
      (if (null? hold)
          lst
          (append lst (list hold)))
      (if (null? last-line)
          (construct hold (last lst) n lst)
          (construct (append hold (list (+ (first last-line) (second last-line)))) (cdr last-line) n lst)))
)

;======================================05=======================================
(define (balanced? in)
  (bal-more? 0 (string->list in))
)

(define (bal-more? par lst)
  (if (or (< par 0) (null? lst))
      (= par 0)
      (if (equal? (car lst) (car(string->list "(")))
          (bal-more? (+ par 1) (cdr lst))
          (if (equal? (car lst) (car(string->list ")")))
              (bal-more? (- par 1) (cdr lst))
              (bal-more? par (cdr lst)))))
)

;======================================06=======================================
(define (list-of-all? predicate lst)
  (foldl eq? #t (map predicate lst))
)

;======================================07=======================================
(define (create-mapping keys vals)
  (lambda (x) (if (not (null? keys))
                  (if (= (length keys) (length vals))
                      (if (symbol? (car keys))
                          (if (equal? (car keys) x)
                              (car vals)
                              ((create-mapping (cdr keys) (cdr vals)) x))
                          "The keys are not all symbols.")
                      "The lists are not of equal length.")
                  (no-map x)))
)

(define (no-map x)
  (string-append "Could not find mapping for symbol '" (symbol->string x))
)
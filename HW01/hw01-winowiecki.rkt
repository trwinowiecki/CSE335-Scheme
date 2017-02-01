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
     for the problem are still present. Otherwise you may be penalized up to 25%
     of the total points for the homework.
   - you may use any number of helper functions you deem necessary.

When done, make sure that you do not get any errors when you hit the "Run" button. You will
lose up to 25% of the total points for the entire homework depending on the number of errors.
If you cannot come up with a correct solution then please make the answer-sheet
compile correctly and comment any partial solution you might have; if this is the case,
the default definitions will have to be present!

|#
;======================================01=======================================
;((3 + 3) * 9)
;equal to 54
(define (p1-1)
  (* 9 (+ 3 3))
)

;((6 * 9) / ((4 + 2) + (4 * 3)))
;equal to 3
(define (p1-2)
  (/ (* 6 9) (+ (+ 4 2) (* 4 3)))
)

;(2* ((20 - (91 / 7)) * (45 - 42)))
;equal to 42
(define (p1-3)
  (* 2 (* (- 20 (/ 91 7)) (- 45 42)))
)
;======================================02=======================================
;write your answer as a string; you do not need to write any special escape
;characters to distinguish new lines.
(define p2
  "You must always start with the operation and then give the operands. It is like a tree that validates the root, left node and then right node."
)
;======================================03=======================================
;;Write the definitions of x,y,z here:
(define x 2)
(define y 3)
(define z 4)
;======================================04=======================================
;you will need to have solved problem 3. The values x,y,z are not parameters
;of this function!
(define (p4)
  (if (= x y z)
      0
      (- (+ x y z) (findSmallest x y z))
  )
)
(define (findSmallest a b c)
  (if (and (< a b) (< a c))
      a
      (if (and (< b a) (< b c))
          b
          c
      )
  )
)

;======================================05=======================================
(define (p5)
  (if (= x y z)
      0
      (- (+ x y z) (findLargest x y z))
  )
)
(define (findLargest a b c)
  (if (and (> a b) (> a c))
      a
      (if (and (> b a) (> b c))
          b
          c
      )
  )
)

;======================================06=======================================
(define (p6)
  (= x y) 
)

;======================================07=======================================
;same instructions as problem 02.
(define p7
  "The first one is defining thirty-five as a variable and the second is defining it as a function."
)

;======================================08=======================================
;same instructions as problem 02.
(define p8
  "It returns whatever follows it as is without computing it."
)

;======================================09=======================================
;same instructions as problem 02.
(define p9
  "A quote is the output format of the function list."
)

;======================================10=======================================
;same instructions as problem 02.
(define p10
  "You can edit a string but not a symbol"
)

;======================================11=======================================
;(4 2 6 9)
(define (p11-1)
  (list 4 2 6 9)  
)

;(spaceship
;  (name(serenity))
;  (class(firefly)))
(define (p11-2)
  (list '(spaceship (name (serenity)) (class (firefly))))
)

;(2 * ((20 - (91 / 7)) * (45 - 42)))
(define (p11-3)
  '(2 * ((20 - (91 / 7)) * (45 - 42)))  
)

;======================================12=======================================
(define example '(a b c))

;(d a b c)
(define (p12-1 lst)
  (cons 'd lst)
)

;(a b d a b)
(define (p12-2 lst)
  (list (first lst) (second lst) 'd (first lst) (second lst))
)

;(b c d a)
(define (p12-3 lst)
  (list (second (reverse lst)) (first (reverse lst)) 'd (first lst))
)


;======================================13=======================================
(define (p13)
  "equal? is true of the two values recursively unfold to the same values and eq? is true of they refer to the same object"
)
; write your answer as a string; you do not need to write any special escape
; characters to distinguish new lines.


;======================================14=======================================
(define (create-error-msg sym val)
  (string-append "This is a custom error message we will be using next. Symbol '" (symbol->string sym) " was not paired with value " (number->string val))
)
;======================================15=======================================
(define (check-correctness pair)
  (if (and (equal? "answer-to-everything" (symbol->string (car pair))) (eq? 42 (cadr pair)))
      #t
      (if (equal? "answer-to-everything" (symbol->string (car pair)))
          "This is a custom error message we will be using next. Symbol 'answer-to-everything was not paired with value 42"
          #f)
      )
)

;======================================16=======================================
;No answer necessary


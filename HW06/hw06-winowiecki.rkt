#lang racket
(#%provide (all-defined))
(#%require (lib "eopl.ss" "eopl"))
(#%require "hw06-env-values.rkt")

;===============================================================================
;========================= Lexical and Grammar Specs ===========================
;===============================================================================

(define lexical-spec
  '(
    (whitespace (whitespace) skip)
    (comment ("#" (arbno (not #\newline))) skip)
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)
    
    (identifier (letter (arbno (or letter digit "_" "-" "?"))) symbol)
    )
  )

;write your answer in string form:
(define problem-1-answer
  "This program skips any whitespace,
				skips comments beginning with '#',
				defines any set of consecutive digits as a number,
				defines any set of consecutive digits following a '-' as a number,
				and defines any set of consecutive letters, digits, '_', '-', and '?' as a symbol"
)

(define grammar-spec
  '(
	 (program (expr (arbno expr)) a-program)
	 
	 (expr (number) num-expr)
	 (expr ("up" "(" expr ")") up-expr)
	 (expr ("down" "(" expr ")") down-expr)
	 (expr ("left" "(" expr ")") left-expr)
	 (expr ("right" "(" expr ")") right-expr)
	 
	 (expr ("(" expr expr ")") point-expr)
	 (expr ("+" expr expr) add-expr)
	 (expr ("origin?" "(" expr ")") origin-expr)
	 (expr ("if" "(" expr ")" "then" expr "else" expr) if-expr)
	 (expr ("move" "(" expr expr (arbno expr) ")") move-expr)

    (expr (identifier) iden-expr)
    (expr ("{" (arbno var-expr) (arbno expr) "}") block-expr)

    (var-expr ("val" identifier "=" expr) val)
    (var-expr ("final" "val" identifier "=" expr) final-val)
  )
)

;given one or more arguments this function will return a flat list
(define (flat-list el1 . rest)
  (flatten (list el1 rest))
)
;===============================================================================
;================================ Value-of =====================================
;=========================================================+
;======================
;value-of takes as a parameter an AST resulted from a call to the
;create-ast function.
(define (run program-string)
  (if (not (string? program-string))
      (raise (string-append "expected a program as string, got: " (~a program-string)))
      (value-of (create-ast program-string) (empty-env))
  )
)

(define (value-of ast env)
  (cond
    [(program? ast) (value-of-program ast env)]
    [(expr? ast) (value-of-expr ast env)]
    [(var-expr? ast) (value-of-var ast env)]
    [else (raise (~a "Unimplemented"))]
  )
)

;for each different ast node type, e.g. <program>, <expr>, <var-expr> you might
;consider implementing a function with the outline:
#|
(define (value-of-ast-node-type ast)
  (cases ast-node-type ast
    (ast-node-type-variant
     (f1 f2)
     'UNIMPLEMENTED
     )
    (else (raise (~a "value-of-ast-node-type error: unimplemented expression: " ast)))
    )
  )
|#

(define (value-of-program ast env)
  (cases program ast
    (a-program (expr rest) (andmap (lambda (e) (value-of e env))
                                   (flat-list expr rest)))
    (else (raise (~a "value-of-program error: unimplemented expression: " ast)))
  )
)

(define (value-of-expr ast env)
  (cases expr ast
    (num-expr (n) (num-val n))
    (up-expr (num) (step-val (up-step (num-val->n (value-of num env)))))
    (down-expr (num) (step-val (down-step (num-val->n (value-of num env)))))
    (left-expr (num) (step-val (left-step (num-val->n (value-of num env)))))
    (right-expr (num) (step-val (right-step (num-val->n (value-of num env)))))

    (point-expr (x y) (point-val (point (num-val->n (value-of x env))
                                        (num-val->n (value-of y env)))))
    (add-expr (e1 e2)
              (letrec
                  ([st1 (step-val->st (value-of e1 env))]
                   [st2 (step-val->st (value-of e2 env))]
                   [st1-n (single-step->n st1)]
                   [st2-n (single-step->n st2)])
                (cond
                  ((and (left-step? st1) (left-step? st2)) (step-val (left-step (+ st1-n st2-n))))
                  ((and (left-step? st1) (right-step? st2)) (if (> st1-n st2-n)
                                                                (step-val (left-step (- st1-n st2-n)))
                                                                (step-val (right-step (- st2-n st1-n)))))
                  ((and (right-step? st1) (right-step? st2)) (step-val (right-step (+ st1-n st2-n))))
                  ((and (right-step? st1) (left-step? st2)) (if (> st1-n st2-n)
                                                                (step-val (right-step (- st1-n st2-n)))
                                                                (step-val (left-step (- st2-n st1-n)))))
                  ((and (down-step? st1) (down-step? st2)) (step-val (down-step (+ st1-n st2-n))))
                  ((and (down-step? st1) (up-step? st2)) (if (> st1-n st2-n)
                                                             (step-val (down-step (- st1-n st2-n)))
                                                             (step-val (up-step (- st2-n st1-n)))))
                  ((and (up-step? st1) (up-step? st2)) (step-val (up-step (+ st1-n st2-n))))
                  ((and (up-step? st1) (down-step? st2)) (if (> st1-n st2-n)
                                                             (step-val (up-step (- st1-n st2-n)))
                                                             (step-val (down-step (- st2-n st1-n))))))
              ))
    (origin-expr (p) (bool-val (equal? (point-val->p (value-of p env)) (point 0 0))))
    (if-expr (e1 e2 e3) (if (bool-val->b (value-of e1 env)) (value-of e2 env) (value-of e3 env)))
    (move-expr (p e rest) (letrec
                              ([start (point-val->p (value-of p env))]
                               [exprs (map (lambda (ex) (value-of ex env)) (flat-list e rest))]
                               [steps (map step-val->st exprs)])
                            (point-val (foldl move start steps))))

    (iden-expr (i) (apply-env env i))
    (block-expr (vars exprs) (andmap (lambda (e) (value-of e (foldl value-of env vars))) exprs))
    (else (raise (~a "value-of-expr error: unimplemented expression: " ast)))
  )
)

(define (value-of-var ast env)
  (cases var-expr ast
    (val (x val) (extend-env-wrapper x (value-of val env) env NON-FINAL))
    (final-val (x val) (extend-env-wrapper x (value-of val env) env FINAL))
    (else (raise (~a "value-of-var error: unimplemented expression: " ast)))
  )
)

(define (move st start-p)
  (cases step st
    (up-step (st) (point (point->x start-p) (+ (point->y start-p) st)))
    (down-step (st) (point (point->x start-p) (- (point->y start-p) st)))
    (left-step (st) (point ( - (point->x start-p) st) (point->y start-p)))
    (right-step (st) (point ( + (point->x start-p) st) (point->y start-p)))
  )
)

;===============================================================================
;============================= sllgen boilerplate ==============================
;===============================================================================
;this will create the AST datatype with define-datatype
;according to the lexical and grammar specifications.
(sllgen:make-define-datatypes lexical-spec grammar-spec)

;you can use this function to display the define-datatype
;expression used to generate the AST. Take some time to read it.
;you should be able to understand it by now.
(define (show-data-types)
  (sllgen:list-define-datatypes lexical-spec grammar-spec))

;create-ast is a one argument function that takes a string,
;scans & parses it and generates a resulting abstract
;syntax tree. 
(define create-ast
  (sllgen:make-string-parser lexical-spec grammar-spec))

;you can use this function to find out more about how
;the string is broken up into tokens during parsing,
;this step is automatically included in the create-ast
;function. This is a one-argument function that takes a 
;string.
(define just-scan
  (sllgen:make-string-scanner lexical-spec grammar-spec))
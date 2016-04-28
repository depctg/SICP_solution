#lang racket
; background code
(define (=number? x num)
  (and (number? x) (eq? x num)))
(define (same-variable? v1 v2)
  (eq? v1 v2))
(define variable? symbol?)
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list '+ a1 a2))))
(define (make-product m1 m2)
  (cond ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((or (=number? m1 0) (=number? m2 0)) 0)
        ((and (number? m1) (number? m2))
         (* m1 m2))
        (else (list '* m1 m2))))
;   pair? -> some kind of "type check?"
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (addend x) (cadr x))
(define (augend x) (caddr x))
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum (make-product
                    (deriv (multiplier exp) var)
                    (multiplicand exp))
                   (make-product
                    (multiplier exp)
                    (deriv (multiplicand exp) var))))
        (else
         (error "unknown expression type -- DERIV" exp))))
; ex2-56
(define (make-exponentiation base exp)
  (cond ((=number? exp 0) 1)
        ; this rule dependent on position
        ((=number? base 0) 0)
        ((=number? exp 1) base)
        (else '(** base exp))))
; ex2-57

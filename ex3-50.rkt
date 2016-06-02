#lang racket
; in racket:
;  delay -> memo-proc 
;   stream-cons
;   stream-first stream-rest
;   stream-ref
;   stream-map is use less

; non-mem-proc
;(define-syntax-rule
; (delay b)
; (lambda () b))
;(define (force x) (x))

(define-syntax-rule
 (stream-cons a b)
 (cons a (delay b)))
(define stream-car car)
(define (stream-cdr s) (force (cdr s)))
(define stream-null? null?)
(define stream-empty '())

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s)
                  (- n 1))))

(define (stream-enumerate-interval s e)
  (if (= s e)
      stream-empty
      (stream-cons s
                   (stream-enumerate-interval (+ s 1)
                                     e))))

(define (stream-filter pred s)
  (if (pred (stream-car s))
      (stream-cons (stream-car s) (stream-filter pred (stream-cdr s)))
      (stream-filter pred (stream-cdr s))))

(define (show x)
  (display x)
  (newline)
  x)

(define (stream-display-n s n)
  (if (= n 0)
      '()
      (begin
        (show (stream-car s))
        (stream-display-n (stream-cdr s) (- n 1)))))

; ex3-50
(define (stream-map op . xs)
  (if (stream-null? (car xs))
      empty-stream
      (stream-cons
       (apply op (map stream-car xs))
       (apply stream-map
              (cons op (map stream-cdr xs))))))

; ex3-51

; (stream-ref x 7) if memo-proc -> 6,7

; ex3-52
; the times of evolution

; defination of integer
(define integer
    (stream-cons 1
                 (stream-map (lambda (x) (+ x 1))
                             integer)))
; gen and apply style
(define fibs
  (letrec [(fibgen
            (lambda (a b)
              (stream-cons b
                           (fibgen b (+ a b)))))]
    (fibgen 0 1)))

; sieve filter
(define (sieve s)
  (stream-cons
   (stream-car s)
   (sieve (stream-filter
           (lambda (x) (not
                        (= 0 (remainder x (stream-car s)))))
           (stream-cdr s)))))
(define primes (sieve (stream-cdr integer)))

; ex3-53
; sn = 2 ^ (n-1)

; ex3-54
(define (stream-mul a b) (stream-map * a b))
(define factorials (stream-cons 1 (stream-mul factorials (stream-cdr integer))))

; ex3-55
(define (partial-sums s)
  (define (repeat x)
    (stream-cons x (repeat x)))
  (stream-cons
   (stream-car s)
   (stream-map +
    (repeat (stream-car s))
    (partial-sums (stream-cdr s)))))
(define (partial-sums-2 s)
  (stream-map +
              s
              (stream-cons 0
                           (partial-sums-2 s))))

; ex3-56
; require s1, s2 dont have continue elements.
; discribe how elements are generated
(define (stream-merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (stream-cons s1car (stream-merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (stream-cons s2car (stream-merge (stream-cdr s2) s1)))
                 (else
                  (stream-cons s1car
                               (stream-merge (stream-cdr s1)
                                      (stream-cdr s2)))))))))
(define (stream-scale s scale)
  (stream-map (lambda (x) (* x scale)) s))
(define S (stream-cons 1 (stream-merge (stream-scale S 2)
                                       (stream-merge (stream-scale S 3)
                                                     (stream-scale S 5)))))

; ex3-57
; linear / tree rec

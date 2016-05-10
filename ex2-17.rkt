#lang racket
; ex2-17
(define (last-pair x)
  (if (null? (cdr x))
      (car x)
      (last-pair (cdr x))))

; ex2-18
(define (reverse x)
  (if (null? x)
      '()
      (append (reverse (cdr x)) (list (car x)))))

; ex2-20
(define (same-parity x . xs)
  (filter (lambda (y)
            (eq? (remainder x 2)
                 (remainder y 2)))
          (cons x xs)))

; ex2-21
(define (square-list l)
  (if (null? l)
      '()
      (cons (* (car l) (car l)) (square-list (cdr l))))
  (map (lambda (x) (* x x)) l))

; ex2-22
; (append answer (list (square (car things))))

; ex2-23
(define (for-each f l)
  (if (null? l)
      #t
      (begin
        (f (car l))
        (for-each f (cdr l)))))
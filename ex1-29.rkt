#lang racket
; public functions
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))
(define (inc x) (+ x 1))
(define (dec x) (- x 1))
(define (id x) x)
;1.30 
(define (iter-sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

; 1.29 辛普森积分
(define (integral f a b n)
  (let ((h (/ (- b a) n)))
    (define (trans-f k)
      (cond ((= k 0) (f a))
            ((even? k) (* 2 (f (+ a (* k h)))))
            (else (* 4 (f (+ a (* k h)))))))
    (* (iter-sum trans-f 0 inc n)
       (/ h 3))))
; 1.31
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))
(define (calc-pi n)
  (define (term-up a)
    (if (odd? a)
        (+ a 1)
        (term-up (inc a))))
  (define (term-down a)
    (if (even? a)
        (+ a 1)
        (term-down (inc a))))
  (* 4 (/ (product term-up 1 inc n)
     (product term-down 1 inc n))))
; 1.32 这个象有点抽..
(define (accumulate combainer null-value term a next b)
  (if (> a b)
      null-value
      (combainer (term a)
                 (accumulate combainer null-value term (next a) next b))))
(define (accumulate-sum term a next b)
  (accumulate + 0 term a next b))
; 1.33
(define (accumulate-filter filter combainer null-value term a next b)
  ; 这个a用的很6啊...理论上保证了没问题形式上还很正确...
  (define (iter a result)
    (let ((t (term a)))
      (if (> a b)
          result
          (if (filter t)
              (iter (next a) (+ result t))
              (iter (next a) result)))))
  (iter a null-value))
(define (accumulate-filter-sum filter term a next b)
  (accumulate-filter filter + 0 term a next b))
; Test case
; (integral (lambda (x) x) 1 2 10000)
(exact->inexact (calc-pi 100000))
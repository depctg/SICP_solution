#lang racket

(define (f-r x) 
  (cond ((< x 3) x)
       (else (+ (* 2 (f-r (- x 2))) (f-r (- x 1)) (* 3 (f-r (- x 3)))))))
(define (f-i x)
  (define (iter i fn-1 fn-2 fn-3)
    (if (= i x)
      (+ fn-1 (* 2 fn-2) (* 3 fn-3))
      (iter (+ i 1) (+ fn-1 (* 2 fn-2) (* 3 fn-3)) fn-1 fn-2)))
  (cond ((< x 3) x)
        (else (iter 3 2 1 0))))

(f-r 4)
(f-i 4)

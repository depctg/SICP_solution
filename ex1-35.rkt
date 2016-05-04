(define tolerance 0.00001)

(define (fix-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? next guess)
          next
          (try next))))
  (try first-guess))

; ex 1.35
;(display (fix-point (lambda (x) (+ 1 (/ 1 x))) 1.0))
;(newline)
;(newline)
; ex 1.36
(define (fix-point-print f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display next)
      (newline)
      (if (close-enough? next guess)
          next
          (try next))))
  (try first-guess))
;(fix-point-print (lambda (x) (/ (log 1000) (log x))) 2.0)
;(newline)
;(fix-point-print (lambda (x) (/ (+ (/ (log 1000) (log x)) x) 2)) 2.0)
;ex 1.37
(define (cont-frac fn fd t)
  ; 注意闭包性质
  (define (rec i)
    (if (= i (+ t 1))
        0
        (/ (fn i) (+ (fd i) (rec (+ i 1))))))
  (define (iter i acc)
    (if (= i 0)
        acc
        (iter (- i 1)
              (/ (fn i) (+ acc (fd i))))))
  (iter t 0))

;(display (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 1000))
; ex 1.38
(define (fd-euler i)
  (let ((r (remainder i 3)))
       (if (= r 2)
           (* 2 (/ (+ i 1) 3))
           1)))
;(display (+ (cont-frac (lambda (i) 1.0) fd-euler 100000) 2))

; ex 1.39
(define (tan-cf x k)
  (define (fn i) 
    (if (= i 1)
        x
        (- (* x x))))
  (define (dn i) 
    (- (* i 2) 1))
  (exact->inexact (cont-frac fn dn k)))
(display (tan-cf 10 1000))

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

(define (deriv g)
  (let ((dx 0.000001))
       (lambda (x)
         (/ (- (g (+ x dx)) (g x))
            dx))))
; g(x) = 0 -> f(x) = x :: g(x) => f(x)
(define (newton-transform g)
  (lambda (x)
    (- x
       (/ (g x) ((deriv g) x)))))
(define (newton-method g guess)
  (fix-point (newton-transform g) guess))

(define (sqrt x)
  (newton-method (lambda (y) (- (* y y) x))
                 1.0))

;(display (sqrt 2))

; ex 1.40
(define (cubic a b c)
  (lambda (x) 
    (+ (* x x x)
       (* a (* x x))
       (* b x)
       c)))
;(display (newton-method (cubic 3 2 1) 1.0))

; ex 1.41
; 包装器云云?
(define (double f) (lambda (x) (f (f x))))
(define (inc x) (+ x 1))
;(display (((double double) inc) 5))
; db (db db) = db . (db . db)
; 组合的是db而不是db . inc

;ex 1.42
(define (compose f g) (lambda (x) (f (g x))))
    ; 这里作为运算符的意义减少了一些
(define (square x) (* x x))
;(display ((compose square inc) 5) )

;ex 1.43
(define (repeat f n)
  (if (= n 1)
      f
      (compose f (repeat f (- n 1)))))
;(display ((repeat square 2) 5))

; ex 1.44
(define (smooth f)
  (define dx 0.001)
  (lambda (x)
    (/ (+ (f (- x dx))
          (f x)
          (f (+ x dx)))
       3)))
(define (smooth-n f n)
  ((repeat smooth n) f))

; ex 1.45
(define (iterative-improve improve good-enough?)
  (define (rec x)
    ; 参数的角度: currying 迭代
    (let ((next (improve x)))
         (if (good-enough? x next)
             next
             (rec next))))
  rec)
; 好像有什么语法能够简单一点来的..
; 直接返回.\
; 用.\只不过是把函数的名字整得更奇怪的一点。用到了母函数的返回。 当然我们有Y F = F . Y F 会漂亮一点
; 闭包的封装已经够了...没有必要再进行了
(define (fix-point f first-guess)
  ((iterative-improve f
                      (lambda (x y)
                        (< (abs (- x y))
                           0.00001)))
   first-guess))
(display (fix-point cos 1.0))

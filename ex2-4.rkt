#lang racket
(define (cons x y)
  (lambda (m) (m x y)))
(define (car z)
  (z (lambda (x y) x)))
(define (cdr z)
  (z (lambda (x y) y)))
; ex2-5
; 歌德尔数, https://zh.wikipedia.org/wiki/%E5%93%A5%E5%BE%B7%E5%B0%94%E6%95%B0
; ex2-6
; 至少比冯诺依曼表示简单一些
; zero -> f(x) => Id -> f^0
; n+ -> f(x) => f $ n f $ x -> f^(n+1)
; f 有点像一个算子...或许没有任何意义...关于x的函数必须定义在这个算子之下 f确定操作也确定 可以称之为:后继函数
(define zero (lambda (f) (lambda (x) x)))
(define (add-one n)
  (lambda (f) (lambda (x) (f ((n f) x)))))
(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))
(define (add n1 n2)
  (lambda (f) (lambda (x) ((n1 f) ((n2 f) x)))))
(define (print-church x)
  (display ((x (lambda (x) (+ x 1))) 0)))
; other encoding method & product & sub
; https://www.zhihu.com/question/19804597
; http://blog.csdn.net/sedgewick/article/details/5796490 Y-combainer..
(define (product n1 n2)
  (lambda (f) (n1 (n2 f))))
; Bouns : Y-Combainer
(define Y
  (lambda (f)
    ((lambda (x) (f x x))
     (lambda (x) (f x x)))))
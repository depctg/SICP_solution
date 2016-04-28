#lang racket
(define (add-poly p1 p2)
(if (same-variable? (variable p1) (variable p2))
    (make-ploy (variable p1)
               (add-terms (term-list p1)
                          (term-list p2)))
    (error "Ploys not in same var -- ADD-PLOY"
           (list p1 p2))))
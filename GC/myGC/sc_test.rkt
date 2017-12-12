#lang plai/mutator
(allocator-setup "sc.rkt" 40)

(cons 6 'a)
(cons 6 'a)
(define x (cons 7 9))
(define (func) (lambda (x1) (cons x x1)) 8)
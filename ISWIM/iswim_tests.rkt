#lang racket

(require "iswim.rkt")

;(delta '(+))
;(delta '(+ 1 2))
;(delta '(- 1 2))
;(delta '(* 1 2))
;(delta '(/ 1 2))
;(delta '())
;(delta '(^ 1 2))

;(substitute (hash 'x 10) 'x)
;(substitute (hash 'x 10) 'y)
;(substitute (hash 'x 10) 20)
;(substitute (hash 'x 10) '((lambda (x) x) 20))
;(substitute (hash 'x 10) '((lambda (y) y) x))
;(substitute (hash 'x 10) '(+ x 20))
;(substitute (hash 'y 10) '(+ x 20))

;(interpret (hash) 10)
(interpret (hash) 'x)
(interpret (hash 'x 10) 'x)
(interpret (hash) '(lambda (x) x))
(interpret (hash) '((lambda (x) x) 10))
(interpret (hash) '((lambda (y) y) 10))
(interpret (hash) '((lambda (x) y) 10))
(interpret (hash) '((lambda (x) y) x))
(interpret (hash 'x 10) '((lambda (x) x) 20))
(interpret (hash 'x 10) '((lambda (x) x) 20))
(interpret (hash) '(+ 1 2))
(interpret (hash 'x 10) '(+ x 20))
(interpret (hash 'x 10) '(+ x ((lambda (x) x) 20)))


;try program that tests overshadowing


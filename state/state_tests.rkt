#lang racket

(require "state2.rkt")

;(eval-interp '(+ 1 2))
(eval-interp '(+ 1 x))
;(interp (hash) 10)
;(interp (hash) 'x)
;(interp (hash 'x 10) 'x)
;(interp (hash) '(lambda (x) x))
;(interp (hash) '((lambda (x) x) 10))
;(interp (hash) '((lambda (y) y) 10))
;(interp (hash) '((lambda (x) y) 10))
;(interp (hash) '((lambda (x) y) x))
;(interp (hash 'x 10) '((lambda (x) x) 20))
;(interp (hash 'x 10) '((lambda (x) x) 20))
;(interp (hash) '(+ 1 2))
;(interp (hash 'x 10) '(+ x 20))
;(interp (hash 'x 10) '(+ x ((lambda (x) x) 20)))


;try program that tests overshadowing


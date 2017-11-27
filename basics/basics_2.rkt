;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname basics_2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;(provide (all-defined-out))

; Combinator
; make sure it's call by value
(define Y (lambda (f)
            ((λ (x) (f (x x))) (λ (x) (f (x x))))))

; Control flow expressions
(define If (lambda (b)
             (lambda (t)
               (lambda (f)
                 ((b t) f)))))
(define if0 (lambda (n)
              ((n (λ (x) f)) t)))

(define lte (lambda (m n)
              (if0 (sub m n))))

(define t (lambda (x)
            (lambda (y)
              x)))

(define f (lambda (x)
            (lambda (y)
              y)))

; Data structures
(define pair (lambda (m n)
               (lambda (s)
                 ((s m) n))))
(define fst (lambda (p)
              (p t)))
(define snd (lambda (p)
              (p f)))

; Arithmetic
(define add-one (lambda (n)
                  (lambda (f)
                    (lambda (x)
                      (f ((n f) x))))))
(define add (lambda (m n)
              ((m add-one) n)))


(define trans (lambda (p)
                (pair (snd p) (add-one (snd p)))))
(define sub-one (lambda (n)
                  (fst ((n trans) (pair zero zero)))))
(define sub (lambda (m n)
              ((m sub-one) n)))



; Utilities
(define inc (lambda (x) (+ x 1)))
(define mkNum (lambda (f)
                ((f inc) 0)))

; Numbers
(define zero (lambda (f)
               (lambda (x)
                 x)))
(define one (lambda (f)
              (lambda (x)
                (f x))))
(define two (add-one one))
(define four (add two two))

; Fibonacci
(define mkfib
  (lambda (fib)
    (lambda (x)
      (((If (lte one x))
        x)
       (add
        (fib (sub-one x))
        (fib (sub-one (sub-one x))))))))

(define (fib-dumb x)
  (if (< x 2)
      x
      (+ (fib-dumb (- x 1)) (fib-dumb (- x 2)))))

(lte zero one)
(((lte zero one) 0) 1)
(((If t) 0) 1)
(((If f) 0) 1)
(add
 (sub-one two)
 (sub-one (sub-one two)))


(((If (lte two one))
  two)
 (add
  (sub-one two)
  (sub-one (sub-one two))))
(mkNum (((If (lte one two))
         two)
        (add
         (sub-one two)
         (sub-one (sub-one two)))))

;(Y mkfib)
;(fib zero)
;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname basics) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


; Utility functions
(define inc (lambda (x)
              (+ x 1)))

; Lambda expressions
(define zero (lambda (f)
               (lambda (x)
                 x)))
;(define (one) (lambda (f) (lambda (x) ((f) x))))
;(define (two f x) (f (f x)))

(define add-one (lambda (n)
                  (lambda (f)
                    (lambda (x)
                      (f ((n f) x))))))
(define add (lambda (m)
              (lambda (n)
                ((m add-one) n))))

(define fib (lambda (f)
              (lambda (n)
                (if (<= n 1)
                    n
                    (+ (f (- n 1)) (f (- n 2)))))))

(require redex)
(require redex/tut-subst)

(collection-file-path "tut-subst.rkt" "redex")

(define one (add-one zero))
(define two (add-one one))
(define three ((add one) two))
(define false (lambda (x)
                (lambda (y)
                  y)))
(define true (lambda (x)
                (lambda (y)
                  x)))
(define if (lambda (v)
             (lambda (t)
               (lambda (f)
                 ((v t) f)))))
((true 0) 1)
((false 0) 1)
(((if true) 0) 1)
(((if false) 0) 1)

(((add-one (add-one zero)) inc) 0)
(define Y (lambda (f)
            ((f (lambda (x) (x x))) (f (lambda (x) (x x))))))
(define pair (lambda (s)
               (lambda (x)
                 (lambda (y)
                   ((s x) y)))))
((pair one) two)
((((pair one) three) inc) 0)
(define tstPair ((pair one) two))
(define fst (lambda (s)
              (true)))
(fst tstPair)
(((fst tstPair) inc) 0)



;(define ( n f x) (f (n f x)))
;(define (add n m)
;  (λ (f x) (m (add-one n f x))))

;; Church numeral representation of 0.
;(define zero (λ (f) (λ (x) x)))

;; Returns the next Church numeral after n.
;(define (succ n)
;  (λ (f) (λ (x) (f ((n f) x)))))

;; Church numeral for 1.
;(define one (succ zero))

;; Adds two Church numerals.
;(define (add n m)
; (λ (f) (λ (x) ((n f) ((m f) x)))))


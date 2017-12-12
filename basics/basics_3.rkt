#lang lazy

;factorial function

(define zero (lambda (f)
               (lambda (x)
                 x)))

(define addOne (lambda (n)
                 (lambda (f)
                   (lambda (x)
                     (f ((n f) x))))))

(define inc (lambda (x) (+ x 1)))
(define mkNum (lambda (f)
                ((f inc) 0)))

;(mkNum zero)

(define one (addOne zero))

;(mkNum one)

(define add (lambda (m)
              (lambda (n)
                ((m addOne) n))))

;(mkNum ((add one) one))

(define factNaive (lambda (x)
                    (if (eq? x 1)
                        x
                        (* x (factNaive (- x 1))))))

; should be
; 2,432,902,008,176,640,000
; 2,432,902,008,176,640,000
;(factNaive 20)

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

(define trans (lambda (p)
                (pair (snd p) (addOne (snd p)))))
(define sub-one (lambda (n)
                  (fst ((n trans) (pair zero zero)))))
(define sub (lambda (m)
              (lambda (n)
                ((m sub-one) n))))

;(mkNum ((sub one) one))



(define If (lambda (b)
             (lambda (t)
               (lambda (f)
                 ((b t) f)))))



(define mult (lambda (m)
               (lambda (n)
                 ((m (add n)) zero))))


;(mkNum ((mult one) one))
(define two (addOne one))

;(mkNum two)
;(mkNum ((mult one) two))
;(mkNum ((mult two) one))

(define three (addOne two))
;(mkNum three)
;(mkNum ((mult two) three))
;(mkNum ((mult three) two))

;(mkNum (sub-one three))

;(mkNum (((If t) zero) one))
;(mkNum (((If f) zero) one))

(define If0 (lambda (n)
              ((n (λ (x) f)) t)))

;(printf "Nested Mult: ~a\n" (mkNum ((mult one) ((mult two) three))))

; using named functions
(define factChurch (lambda (x)
                     (begin
                       ;(printf "Entering: ~a\n" (mkNum x))
                       (((If (If0 x))
                           (begin
                             ;(printf "Returning ~a\n" (mkNum x))
                             one))
                           (begin
                             ;(printf "Recursing ~a, passing ~a\n" (mkNum x) (sub-one x))
                             ((mult x) (factChurch (sub-one x))))))))


;(mkNum ((lambda (x)
;          (begin
;            (printf "~a\n" (mkNum x))
;            (sub-one x)))
;        three))
;(mkNum (((If0 zero) zero) one))
;(mkNum (((If0 one) zero) one))
;(mkNum (((If0 two) zero) one))

(define four (addOne three))

(mkNum (factChurch zero))
(mkNum (factChurch one))
(mkNum (factChurch two))
(mkNum (factChurch three))
(mkNum (factChurch four))


; using anonymous functions
(define mkFact (lambda (f)
                 (lambda (x)
                     (begin
                       ;(printf "Entering: ~a\n" (mkNum x))
                       (((If (If0 x))
                           (begin
                             ;(printf "Returning ~a\n" (mkNum x))
                             one))
                           (begin
                             ;(printf "Recursing ~a, passing ~a\n" (mkNum x) (sub-one x))
                             ((mult x) (f (sub-one x)))))))))

(define Y (lambda (f)
              ((lambda (x) (f (x x))) (lambda (x) (f (x x))))))

(mkNum ((Y mkFact) zero))
(mkNum ((Y mkFact) one))
(mkNum ((Y mkFact) two))
(mkNum ((Y mkFact) three))
(mkNum ((Y mkFact) four))

;(mkNum (factChurch two))
#lang racket

(define (delta op vs)
  (match op
    ['+ (apply + vs)]
    ['- (apply - vs)]
    ['* (apply * vs)]
    ['/ (apply / vs)]
    [else (error 'delta "unrecognized operator ~a\n" op)]))

(struct :clo (e x m) #:transparent)
(struct :k:fun (a e k) #:transparent)
(struct :k:arg (f k) #:transparent)
(struct :k:prm (o vs e ms k) #:transparent)
(struct :k:mt () #:transparent)

(define (cek c e k)
  (eprintf "c = ~a, e = ~a, k = ~a\n" c e k)
  (match c
    [`(lambda ,x ,b) (cek (:clo e x b) e k)]
    [`(,M ,N) (cek M e (:k:fun N e k))]
    [`(,op ,ms ...) (let ([vs (map (lambda (x) (cek x e k)) ms)])
                      (if (andmap number? vs)
                          (delta op vs)
                          c))]
    [symbol? #:when (hash-has-key? e c) (cek (hash-ref e c) e k)]
    [else
     (match k
       [(:k:mt) c]
       [(:k:fun a e k) (cek a e (:k:arg c k))]
       [(:k:arg (:clo clo-e x m) k) (cek m (hash-set clo-e x c) k)])]))

(define (eval-cek c)
  (cek c (hasheq) (:k:mt)))

(eval-cek '((lambda x x) 10))
(eval-cek '(+ 12 23 34 45))
(eval-cek '((lambda x (+ 12 23 34 x)) 10))
(eval-cek '(((lambda x (lambda y (+ x y))) 10) 11))
(eval-cek '(((lambda x (lambda x (+ x x))) 10) 11))
(eval-cek '(((lambda x (lambda x (+ x z))) 10) 11))
(eval-cek '(+ 10 20 (((lambda x (lambda x (+ x z))) 10) 11)))

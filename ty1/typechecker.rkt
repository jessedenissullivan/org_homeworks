#lang racket

(define (infer-type env c)
  (eprintf "c = ~a\n" c)
  (cond
    [(number? c) 'num]
    [(boolean? c) 'bool]
    [(symbol? c) (if (hash-has-key? env c)
                     (hash-ref env c)
                     (let ([new-id (gensym)])
                       (hash-set! env c new-id)
                       new-id))]
    [else (match c
            [`(lambda (,x) (,b)) (cons (infer-type env x) (infer-type env b))]
            [`(,M ,N) (let ([fun-type (infer-type env M)]
                            [arg-type (infer-type env N)])
                        (cond [(not (hash-has-key? env (car fun-type))) ...]
                              [(eq? (hash-ref env (car fun-type)) 'num) ...]
                              [(eq? (hash-ref env (car fun-type)) 'bool) ...]
                              [else ...]
                              ))])]))

(define (eval-infer-type c)
  (infer-type (make-hash) c))

(eval-infer-type '((lambda (x) (x)) 10))
(eval-infer-type '((lambda (x) (x)) #t))
(eval-infer-type '((lambda (x) (x)) 'y))

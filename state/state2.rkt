#lang racket

(provide delta
         get_ty
         eval-interp)

(define (ty-check b gamma)
  (cond 
    [(symbol? b) (hash-ref gamma b (lambda () (eprintf "type of ~a not known\n" b)))]
    [(number? b) 'num]
    [(boolean? b) 'bool]
    [else (match b
            [`(,m ,n) (let ([m_ty (get_ty m gamma)]
                            [n_ty (get_ty n gamma)])
                        (if (eq? (car m_ty) n_ty)
                            (cdr m_ty)
                            (error 'get_ty "~a is a type violation" b)))]
            [`(lambda (,x : ,t) ,b) (get_ty b gamma)])]))

(define (delta op ms)
  (match op
    ['+ (apply + ms)]
    ['- (apply - ms)]
    ['/ (apply / ms)]
    ['* (apply * ms)]))

(define (eval-get-ty c)
  (get_ty c (hasheq)))

(define (interp c env)
  (match c
    [`(lambda (,x : ,t) ,b) c]
    [`(,m ,n) (let ([m_ty (eval-get-ty m )]
                    [n_ty (eval-get-ty n)])
                (if (eq? (car m_ty) n_ty)
                    (cdr m_ty)
                    (error 'get_ty "~a is a type violation" c)))]
    [`(,op ,ms ...) (let ([ms_ty (map (lambda (x) (eval-get-ty x)) ms)])
                      (eprintf "op = ~a, ms_ty = ~a\n" op ms_ty)
                      (if (andmap (lambda (x) (eq? 'num x)) ms_ty)
                          (delta op ms)
                          (error 'get_ty "~a is a type violation" c)))]
    [symbol? (hash-ref env c)]
    [else c]))

(define (eval-interp c)
  (interp c (hasheq)))
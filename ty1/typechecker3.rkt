#lang racket

(define cntr 0)
(define (gensym sym)
  (set! cntr (+ cntr 1))
  (string->symbol (format "~a~a" sym cntr)))

(define (get-const gamma c)
  (eprintf "gamma = ~a, c = ~a\n" gamma c)
  (cond [(number? c) (values 'num '() '())]
        [(boolean? c) (values 'bool '() '())]
        [(symbol? c) (let ([var_ty (hash-ref gamma c)])
                       (values var_ty '() '()))]
        [else (match c
                [`(lambda (,x) ,b) (letrec-values ([(a_ty) (hash-ref! gamma x (lambda () (gensym 'alpha)))]
                                                   [(b_ty b_const b_vars) (get-const gamma b)]
                                                   [(ret_ty) `(-> ,a_ty ,b_ty)]
                                                   [(ret_ty_var) (gensym 'alpha)])
                                     (eprintf "a_ty = ~a, b_ty = ~a, b_const = ~a, b_vars = ~a, ret_ty = ~a\n" a_ty b_ty b_const b_vars ret_ty)
                                     (values ret_ty_var (append b_const `((= ,ret_ty_var ,ret_ty))) (append b_vars `(,a_ty))))]
                [`(,M ,N) (letrec-values ([(m_ty m_const m_vars) (get-const gamma M)]
                                          [(n_ty n_const n_vars) (get-const gamma N)]
                                          [(ret_ty) (gensym 'alpha)])
                            (eprintf "m_ty = ~a, m_const = ~a, m_vars = ~a, n_ty = ~a, n_const = ~a, n_vars = ~a, ret_ty = ~a\n" m_ty m_const m_vars n_ty n_const n_vars ret_ty)
                            (values ret_ty
                                    (append m_const n_const `((= ,m_ty (-> ,n_ty ,ret_ty))))
                                    (append m_vars n_vars `(,ret_ty))))])]))

(define (eval-get-const c)
  (get-const (make-hash) c))

(eval-get-const 10)
(eval-get-const #t)
(eval-get-const '(lambda (x) x))
(eval-get-const '((lambda (x) x) 10))
(eval-get-const '((lambda (x) x) ((lambda (y) y) 10)))
(eval-get-const '(((lambda (x) x) (lambda (y) y)) 10))
(eval-get-const '(lambda (x) (x x)))

(define (unify constraints env)
  (eprintf "constraints = ~a\n" constraints)
  (eprintf "---------------------\nenv = ~a\n" env)
  (cond [(eq? (length constraints) 0) (printf "constraints are satisfied\n")
                                      env]
        [else (match (car constraints)
                [`(= (-> ,t1 ,t2) (-> ,t3 ,t4))  ;; relook at all the constraints, just changed them                                        
                 (unify
                  (append (cdr constraints) `((= ,t1 ,t3)) `((= ,t2 ,t4)))
                  env)]
                [`(= ,t1 ,t2) (eprintf "t1 = ~a, t2 = ~a\n" t1 t2)
                              (cond [(eq? t1 t2) (unify (cdr constraints) env)] ;; tautology, ignore
                                    [(and (eq? t1 'num)
                                          (symbol? t2)) (unify (cdr constraints) (hash-set env t2 t1))]
                                    [(and (eq? t2 'num)
                                          (symbol? t1)) (unify (cdr constraints) (hash-set env t1 t2))]
                                    [(and (eq? t1 'bool)
                                          (symbol? t2)) (unify (cdr constraints) (hash-set env t2 t1))]
                                    [(and (eq? t2 'bool)
                                          (symbol? t1)) (unify (cdr constraints) (hash-set env t1 t2))]
                                    [(and (symbol? t1)
                                          (not (symbol? t2))) (if (is-not-in-free-vars-of t2 t1)
                                                                  (unify (cdr constraints) (hash-set env t1 t2))
                                                                  (error 'unify "type violation"))]
                                    [(and (symbol? t2)
                                          (not (symbol? t1))) (if (is-not-in-free-vars-of t1 t2)
                                                                  (unify (cdr constraints) (hash-set env t2 t1))
                                                                  (error 'unify "type violation"))]
                                    [(and (symbol? t1)
                                          (symbol? t2)) (let ([t1_ty (hash-ref env t1 (lambda () t1))]
                                                              [t2_ty (hash-ref env t2 (lambda () t2))])
                                                          (unify (append (cdr constraints) `((= ,t1_ty ,t2_ty))) env))])])]))

(define (is-not-in-free-vars-of t2 t1)
  (let ([free-vars-of-t2 (get-free-vars t2)])
    (if (eq? #f (member t1 free-vars-of-t2))
        #t
        #f)))

(define (get-free-vars expr)
  (match expr
    [`(-> ,t1 ,t2) (append (get-free-vars t1)
                           (get-free-vars t2))]
    [else (list expr)]))

;(get-free-vars '(m n))
;(get-free-vars '(lambda (x) x))
;(get-free-vars '((lambda (m) (+ m n)) a))
;(get-free-vars '((lambda (m) (+ m ((lambda (m) (+ m 1)) 2))) a))
;(get-free-vars '((lambda (m) (+ m n ((lambda (n) (+ n 1)) 2))) a))
;(member 'a (get-free-vars '((lambda (m) (+ m n)) a)))

;(unify '((= (-> a1 a1) (-> a3 a4))) (make-hash) 0)
;(unify '((= (-> a1 a1) (-> a3 a4)) (= a1 num)) (make-hash) 0)
;(unify '((= alpha1 (-> alpha1 alpha2))) (make-hash) 0)

;(map (lambda (t2) (eq? 'alpha1 t2)) '((-> 'alpha1 'alpha2)))

;(unify '((= alpha1 (-> alpha1 alpha2))) (make-hash) 0)

(define (eval-unify c)
  (unify c (hasheq)))

(define (infer-typecheck c)
  (letrec-values ([(ty const vars) (eval-get-const c)]
                  [(env) (eval-unify const)])
    (eprintf "The program typechecks\n")))

(infer-typecheck '((lambda (x) x) ((lambda (y) y) 10)))
;(infer-typecheck '(4 10))
;(infer-typecheck '(lambda (x) (x x)))
;(infer-typecheck '((lambda (x) (x x)) (lambda (x) (x x))))
(infer-typecheck '(lambda (x) x))
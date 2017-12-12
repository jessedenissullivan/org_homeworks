#lang racket

(define env (make-hash))
(define cntr 0)
(define (gensym sym)
  (set! cntr (+ cntr 1))
  (string->symbol (format "~a~a" sym cntr)))

(define (get-constraints c constraints vars)
  (eprintf "c = ~a, const = ~a, vars = ~a\n" c constraints vars)
  (cond [(symbol? c) (if (hash-has-key? env c)
                         (values (hash-ref env c) constraints vars)
                         (let ([new_ty (gensym 'alpha)])
                           (hash-set! env c new_ty)
                           (values new_ty constraints (append vars (list new_ty)))))]
        [(number? c) (values 'num constraints vars)]
        [(boolean? c) (values 'bool constraints vars)]
        [else (match c
                [`(lambda (,x) (,b)) (let-values ([(x_ty x_cons x_vars) (get-constraints x constraints vars)]
                                                  [(b_ty b_cons b_vars) (get-constraints b constraints vars)])
                                       (eprintf "x_ty = ~a, x_cons = ~a, x_vars = ~a\n" x_ty x_cons x_vars)
                                       (eprintf "b_ty = ~a, b_cons = ~a, b_vars = ~a\n" b_ty b_cons b_vars)
                                       (values `(-> ,x_ty ,b_ty)
                                               (append constraints x_cons b_cons `((-> ,x_ty ,b_ty)))
                                               (append vars x_vars b_vars)))]
                [`(,M ,N) (let-values ([(m_ty m_cons m_vars) (get-constraints M constraints vars)]
                                       [(n_ty n_cons n_vars) (get-constraints N constraints vars)]
                                       [(new_ty) (gensym 'alpha)])
                            ;(eprintf "m_ty = ~a, m_cons = ~a, m_vars = ~a\n" m_ty m_cons m_vars)
                            ;(eprintf "n_ty = ~a, n_cons = ~a, n_vars = ~a\n" n_ty n_cons n_vars)
                            (values new_ty
                                    (append constraints m_cons n_cons `((-> ,m_ty (-> ,n_ty ,new_ty))))
                                    (append vars m_vars n_vars `(,new_ty))))])]))

(define (eval-get-constraints c)
  (get-constraints c '() '()))

(eval-get-constraints '((lambda (x) (x)) 10))

(define (unification constraints vars)
  (eprintf "const = ~a, vars = ~a\n" constraints vars))

(define (eval-unification c)
  (let-values ([(ty constraints vars) (eval-get-constraints c)])
    (unification constraints vars)))

(eval-unification '((lambda (x) (x)) 10))
(eval-unification  '(((lambda (x) (x)) (lambda (y) (y))) 10))
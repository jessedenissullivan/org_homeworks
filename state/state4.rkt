#lang racket

(define (ty-check c gamma)
  (eprintf "ty-check: c = ~a, gamma = ~a\n" c gamma)
  (match c
    [`(lambda (,x : ,ty) ,m) (hash-set! gamma x ty)
                             (letrec ([m_ty (ty-check m gamma)])
                               `(-> ,ty ,m_ty))]
    [`(,m ,n) (eprintf "m = ~a, n = ~a\n" m n)
              (let ([m_ty (ty-check m gamma)]
                    [n_ty (ty-check n gamma)])
                (eprintf "c = ~a, m_ty = ~a, n_ty = ~a\n" c m_ty n_ty)
                (eprintf "m = ~a, n = ~a\n" m n)
                (match m_ty
                  [`(-> ,t1 ,t2) (eprintf "c = ~a, gamma = ~a, t1 = ~a, t2 = ~a, n_ty = ~a\n" c gamma t1 t2 n_ty)
                                 (if (equal? t1 n_ty)
                                     t2
                                     (error 'ty-check "~a is a type violation\n" c))]
                  [else (else 'ty-check "Unrecognized type: ~a\n" c)]))]
    [`(seq ,bs ...) (eprintf "in seq, bs = ~a\n" bs)
                    (let ([prev_instrs (reverse (cdr (reverse bs)))]
                          [last_instr (last bs)])
                      (map (lambda (i) (ty-check i gamma)) prev_instrs)
                      (eprintf "last bs = ~a, gamma = ~a\n" last_instr gamma)
                      (ty-check (last bs) gamma))]
    [`(set! ,x ,v) (hash-set! gamma x (ty-check v gamma))
                   (ty-check x gamma)]
    [`(,op ,ms ...) (let ([ms_ty (map (lambda (x) (ty-check x gamma)) ms)])
                      (eprintf "ms_ty = ~a\n" ms_ty)
                      (if (andmap (lambda (x) (eq? x 'num)) ms_ty)
                          'num
                          (error 'ty-check "~a is a type violation\n" c)))]
    [else (cond [(number? c) 'num]
                [(boolean? c) 'bool]
                [(symbol? c) (hash-ref gamma c c)]
                [else (error 'ty-check "unrecognized expression ~a\n" c)])]))

(define (eval-ty-check c)
  (ty-check c (make-hasheq)))

(define (delta op args)
  (eprintf "delta: op = ~a, args = ~a\n" op args)
  (match op
    ['+ (printf "adding\n")(apply + args)]
    ['- (apply - args)]
    ['* (apply * args)]
    ['/ (apply / args)]
    [else (error 'delta "Unrecognized operator: ~a\n" op)]))

;(delta '+ '(1 2 3))
;(delta '^ '(1 2 3))

(define (av e)
  (printf "av: e = ~a\n" e)
  (match e
    [`(lambda (,x : ,ty) ,b) (remove x (av b))]
    [`(set! ,x ,v) (append `(,x) (av v))]
    [`(,m ,n) (append (av m) (av n))]
    [`(,op ,args ...) (printf "in op: op = ~a, args = ~a\n" op args)
                      (flatten (map (lambda (x) (av x)) args))] ;; covers seq case
    [else (cond [(symbol? e) '()]
                [(number? e) '()]
                [(boolean? e) '()]
                [else (error 'av "Unrecognized expression: ~a\n" e)])]))

;(av '((lambda (x : num) x) 10))
;(av '((lambda (x : num) (seq (set! x 20) x)) 10))
;(av '((lambda (x : num) (seq (set! y 20) x)) 10))

(define (in-av x m)
  (let ([av_m (av m)])
    (eprintf "in-av: ~a\n" av_m)
    (if (member x av_m)
        #t
        #f)))

(define (subst e env store)
  (eprintf "subst: e = ~a, env = ~a, store = ~a\n" e env store)
  (match e
    [`(lambda (,x : ,ty) ,b) `(lambda (,x : ,ty) ,(subst b (hash-remove env x) store))]
    [`(set! ,x ,v) `(set! ,(subst x env store) ,(subst v env store))]
    [`(,m ,n) (let ([v (subst n env store)]
                    [f (subst m env store)])
                `(,f ,v))]
    [`(,op ,args ...) `(,op ,@(map (lambda (x) (subst x env store)) args))]
    [else (cond [(symbol? e) (hash-ref env e e)]
                [(number? e) e]
                [(boolean? e) e]
                [else (error 'subst "Unrecognized expression: ~a\n" e)])]))

;(subst '(lambda (x : num) (+ x y)) (hash 'y 1))
;(subst '(lambda (x : num) (+ x y)) (hash 'y 1 'x 2))
;(subst '(lambda (x : num) (+ ((lambda (x : num) x) 10) y)) (hash 'y 1 'x 2))

(define (interp c env)
  (eprintf "interp: c = ~a, env = ~a\n" c env)
  (match c
    [`(lambda (,x : ,ty) ,b) c]
    [`(set! ,x ,v) (let ([u (hash-ref! env x x)])
                     (eprintf "in set! x = ~a, v = ~a\n" x v)
                     (hash-set! env x (interp v env))
                     u)]
    [`((lambda (,x : ,ty) ,b) ,n) (if (in-av x b)                                  
                                      (let ([y (gensym)])
                                        (hash-set! env y (interp n env))
                                        (interp (subst b (hash x y) env) env))
                                      (interp (subst b (hash x n) env) env))]
    [`(seq ,bs ...) (eprintf "in seq, bs = ~a\n" bs)
                    (let ([prev_instrs (reverse (cdr (reverse bs)))]
                          [last_instr (last bs)])
                      (map (lambda (i) (interp i env)) prev_instrs)
                      (eprintf "last bs = ~a, env = ~a\n" last_instr env)
                      (interp (last bs) env))]
    [`(,op ,ms ...) (eprintf "in op, ms = ~a\n" ms)
                    (let ([ms_res (map (lambda (x) (interp x env)) ms)])
                      (delta op ms_res))]
    [else (cond [(symbol? c) (eprintf "var is ~a\n" (hash-ref! env c (lambda ()(error 'interp "Variable not found: ~a\n" c))))
                             (hash-ref! env c (lambda ()(error 'interp "Variable not found: ~a\n" c)))]
                [(number? c) (eprintf "num: ~a\n" c)
                             c]
                [(boolean? c) (eprintf "bool: ~a\n" c)
                              c]
                [else  (error 'interp "Unrecognized expression: ~a\n" c)])]))

(define (eval c)
  (eval-ty-check c)
  (printf "\n")
  (printf "~a\n" (interp c (make-hasheq))))

(eval '(+ 1 2))
(eval '(lambda (x : num) x))
(eval '((lambda (x : num) x) 10))
(eval '(+ 1 ((lambda (x : num) x) 10)))
(eval '(seq (set! x 10) (set! x (+ x 1))))
(printf "\n")
(eval '(seq (set! x 10) (set! x (+ x 1)) ((lambda (y : num) (+ y 20)) x)))

(eval '((lambda (x : num) (seq (set! x 10) (set! x (+ x 1)) ((lambda (y : num) (+ y 20)) x))) 42))
(eval '((lambda (x : num) (seq (set! x 10) ((lambda (y : num) (+ y 20)) x) (set! x (+ x 1)))) 42))
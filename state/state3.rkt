#lang racket

(define (ty-check c gamma)
  (eprintf "c = ~a, gamma = ~a\n" c gamma)
  (match c
    [`(lambda (,x : ,ty) ,m) (let ([m_ty (ty-check m (hash-set gamma x ty))])
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
    [`(let (,bindings ...)
        ,b) (eprintf "new_env = ~a\n" (make-immutable-hash bindings))
            (ty-check b (make-immutable-hash bindings))]
    [`(seq ,bs ...) (last (map (lambda (i) (ty-check)) bs))]
    [`(set! ,x ,v) (ty-check v gamma)]
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
  (ty-check c (hasheq)))

;(eval-ty-check '((lambda (x : num) x) 10))
;(eval-ty-check '((lambda (x : bool) x) 10)) ;; should fail typecheck
;(eval-ty-check '((lambda (x : num) x) #t)) ;; should fail typecheck
;(eval-ty-check '((lambda (x : bool) x) ((lambda (y : bool) y) #t))) 
;(eval-ty-check '((lambda (x : bool) x) (lambda (y : bool) y))) ;; should fail typecheck
;(eval-ty-check '(+ 1 2))
;(eval-ty-check '(+ 1 2 3 4))
;(eval-ty-check '(+ 1 2 3 #t)) ;; should fail typecheck
;(eval-ty-check '(+ 1 2 3 x)) ;; should fail typecheck
;(eval-ty-check '(+ 1 2 3 (lambda (x : num) 4))) ;; should fail typecheck
;(eval-ty-check '(+ 1 2 3 ((lambda (x : num) x) 4)))

(define (delta op vs)
  (eprintf "op = ~a, vs = ~a\n" op vs)
  (match op
    ['+ (apply + vs)]
    ['- (apply - vs)]
    ['* (apply * vs)]
    ['/ (apply / vs)]
    [else (error 'delta "unrecognized operator ~a\n" op)]))

(define (av e)
  (eprintf "e = ~a\n" e)
  (match e
    [`(lambda (,x : ,ty) ,b) (remove x (av b))]
    [`(,m ,n) (append (av m) (av n))]
    [`(set! ,x ,m) (append (av m) `(,x))]
    [`(seq ,es ...) (eprintf "es = ~a\n" es)
                    (remove-duplicates
                     (flatten (map (lambda (x) (av x)) es)))]
    [`(let (,bindings ...)
        ,b) (eprintf "bindings = ~a\n" bindings)
            (remove-duplicates
             (flatten
              (append (av b)
                      (map (lambda (x) (av x)) bindings))))]
    [`(,op ,ms ...) (eprintf "ms = ~a\n" ms)
                    (flatten (map (lambda (x) (av x)) ms))]
    [else (cond [(symbol? e) `(,e)]
                [(boolean? e) '()]
                [(number? e) '()]
                [else (error 'av "unrecognized argument: ~a\n" e)])]))

;(av 'x)
;(av 10)
;(av #t)
;(av '(lambda (x : num) (f x)))
;(av '((lambda (x : num) (f x)) 10))
;(av '((lambda (x : num) (f x)) y))
;(av '((lambda (f : (-> num num)) ((lambda (x : num) (f x)) 10)) y))
;(av '(seq (set! x 20)
;          (set! x (+ x 1)))    )
;(av '(seq (set! x 20)
;          (set! x (+ x 1))
;          (set! y (+ x x)))    )
;(av '(let ([x 4]
;           [y 10])
;       (seq (set! x 20)
;            (set! x (+ x 1))
;            (set! y (+ x x))))    )

(define (interp c env)
  (eprintf "c = ~a, env ~a\n" c env)
  (match c
    [`(lambda (,x : ,ty) ,b) c]
    [`(set! ,x ,v) void]
    [`(let (,bindings ...)
        ,b) (map (lambda (x) (hash-set! env (car x) (last x))) bindings)
            (interp b env)]
    [`(seq ,bs ...) (map (lambda (x) (interp bs env)) bs)]    
    [`(,m ,n) (let ([n_res (interp n env)]
                    [m_res (interp m env)])
                (match m_res
                  [`(lambda (,x : ,t) ,b) (hash-set! env x n_res)
                                          (interp b env)]
                  [else (error 'interp "unrecognized expression: ~a\n" m)]))]
    [`(,op ,ms ...) (eprintf "op = ~a, ms = ~a\n" op ms)
                    (let ([vs (map (lambda (x) (interp x env)) ms)])
                      (delta op vs))]
    [else (cond [(number? c) c]
                [(boolean? c) c]
                [(symbol? c) (let ([new_env (hash-copy env)])
                               (begin (hash-remove! new_env c)
                                      (if (hash-has-key? env c)
                                          (interp (hash-ref env c) new_env)
                                          c)))]
                [else (error 'interp "Unrecognized expression: ~a\n" c)])]))

(define (eval-interp c)
  (if (eval-ty-check c)
      (interp c (make-hash))
      (error 'eval-interp "program does not pass type check")))

(eval-interp '10)
(eval-interp 'x)
(eval-interp '#t)
(eval-interp '(lambda (x : num) x))
(eval-interp '((lambda (x : num) x) 10))
;(eval-interp '((lambda (x : num) x) #t)) ;; should fail due to the type checker
(eval-interp '(+ 1 2 3 4 5))
;(eval-interp '(+ 1 2 3 4 (lambda (x : num) x))) ;; should fail due to the type checker
(eval-interp '(+ 1 2 3 4 ((lambda (x : num) x) 5)))
(eval-interp '(+ 1 2 3 4 ((lambda (x : num) x) ((lambda (x : num) x) 5))))
(eval-interp '(+ 1 2 3 4
                 (((lambda (x : (-> num num)) x)
                   (lambda (x : num) x))
                  5)))

(eval-interp '(let ([x 10])
                x))
(eval-interp '(let ([x 10])
                ((lambda (y : num) y) x)))
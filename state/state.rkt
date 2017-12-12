#lang racket

(provide delta
         interpret
         substitute)

(define delta
  (lambda (e)
    (match e
      [`(,op ,args ...)
       (cond
         [(not (eq? 2 (length args)))
          (error "Used the wrong number of args to delta: " e)]
         [else
          (match op
            ['+ (apply + args)]
            ['* (apply * args)]
            ['- (apply - args)]
            ['/ (apply / args)]
            [else (error "Unrecognized operator: " op)]
            )])]
      [else (error "Unrecognized argument to delta: s" e)])))

(define (substitute env f)
  (match f
    [(? symbol?) (hash-ref env f f)] ; look up val in env ow ret sym
    [(? number?) f]
    [`(lambda ,parms ,body) (let ([new_env (for/fold ([e env])
                                                     ([x parms])
                                             (hash-remove e x))])
                              (substitute new_env body))]
    [`(,op ,ms ...) (let ([vs (map (curry substitute env) ms)])
                      `(,op ,@vs))]))

(define interpret
  (lambda (env e)
    (match e
      [(? number?) e]      ; Values
      [(? symbol?) (hash-ref env e e)]
      [`(lambda ,params ,body) (values params body)]      
      [`(,m ,n) (letrec-values ([(params body) (interpret env m)]
                                [(args) (values (interpret env n))]
                                [(env_p) (for/fold ([env_p env])
                                                   ([arg `(,args)]
                                                    [param params])
                                           (hash-set env_p param arg))])
                  (substitute env_p body))]
      [`(,op ,args ...) (let ([vs (map (curry interpret env) args)])
                          (delta `(,op ,@vs)))]
      [else (raise "Interpreter does not recognize argument")])))

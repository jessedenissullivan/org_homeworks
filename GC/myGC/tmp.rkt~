#lang plai/mutator
(allocator-setup "sc.rkt" 200)
(define (build-one)
  (let* ((x0 1)
         (x1
          (lambda (x)
            (if (= x 0)
              x0
              (if (= x 1)
                x0
                (if (= x 2)
                  x0
                  (if (= x 3)
                    x0
                    (if (= x 4)
                      x0
                      (if (= x 5)
                        x0
                        (if (= x 6)
                          x0
                          (if (= x 7) x0 (if (= x 8) x0 x0)))))))))))
         (x2 0)
         (x3 empty)
         (x4
          (lambda (x)
            (if (= x 0)
              x1
              (if (= x 1)
                x2
                (if (= x 2)
                  x0
                  (if (= x 3)
                    x1
                    (if (= x 4)
                      x0
                      (if (= x 5) x1 (if (= x 6) x1 (if (= x 7) x1 x1))))))))))
         (x5 (cons x0 #f))
         (x6 (cons #f x3))
         (x7 (cons x3 x6))
         (x8
          (lambda (x)
            (if (= x 0)
              x6
              (if (= x 1)
                x6
                (if (= x 2)
                  x2
                  (if (= x 3) x7 (if (= x 4) x6 (if (= x 5) x5 x2)))))))))
    (set-rest! x5 x8)
    (set-first! x6 x8)
    x7))
(define (traverse-one x7) (empty? (first ((first ((first (rest x7)) 4)) 3))))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 200)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 200)

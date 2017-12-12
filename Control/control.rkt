#lang racket

(require racklog)

(%which () %true)
(%which () %fail)

;; define =and2
(define =and2
  (%rel ()
        [(%true %true %true)]
        [(%true %fail %fail)]
        [(%fail %true %fail)]
        [(%fail %fail %fail)]))

(%which () (=and2 %true %true %fail))
(%which () (=and2 %true %true %true))
;; define =or2
;; define =succeed
;; define =fail
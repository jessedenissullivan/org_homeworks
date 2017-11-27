#lang racket

(require plai/random-mutator)

(save-random-mutator "tmp.rkt" "ms_coll.rkt" #:gc2? #f)
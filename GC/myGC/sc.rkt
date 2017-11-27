#lang plai/collector

;; a stop and copy garbage collector adpating the work of Michael Bernstein
;; (https://gist.github.com/mrb/5617138)

;; functions written by Michael Bernstein

;; Initialize allocator. Iterate over all of the cells in the heap,
;; and mark them 'free
(define (init-allocator) 
  (for ([i (in-range 0 (heap-size))])
    (heap-set! i 'free)))

;; Dereference the cell location, return the value if it is flat. 
(define (gc:deref loc) 
  (cond
    [(equal? (heap-ref loc) 'flat)
     (heap-ref (+ loc 1))]
    [else
     (error 'gc:deref "attempted to deref a non flat value, loc ~s" loc)]))

;; Return the first value in the cell if it stores a pair
(define (gc:first pr-ptr) 
  (if (equal? (heap-ref pr-ptr) 'pair)
      (heap-ref (+ pr-ptr 1))
      (error 'first "non pair")))

;; Return the rest value in the cell if it stores a pair
(define (gc:rest pr-ptr) 
  (if (equal? (heap-ref pr-ptr) 'pair)
      (heap-ref (+ pr-ptr 2))
      (error 'first "non pair")))

;; Does this cell store a flat value?
(define (gc:flat? loc) (equal? (heap-ref loc) 'flat))

;; Does this cell store a pair?
(define (gc:cons? loc) (equal? (heap-ref loc) 'pair))

;; Set the first value in a pair
(define (gc:set-first! pr-ptr new) 
  (if (equal? (heap-ref pr-ptr) 'pair)
      (heap-set! (+ pr-ptr 1) new)
      (error 'set-first! "non pair")))

;; Set the rest value in a pair
(define (gc:set-rest! pr-ptr new) 
  (if (equal? (heap-ref pr-ptr) 'pair)
      (heap-set! (+ pr-ptr 2) new)
      (error 'set-first! "non pair")))

;; Allocate a flat value, store it on the heap
(define (gc:alloc-flat fv) 
  (let ([ptr (alloc 2)])
    (heap-set! ptr 'flat)
    (heap-set! (+ ptr 1) fv)
    ptr))

;; Cons two allocated values and store the list on the heap
(define (gc:cons hd tl)
  (eprintf "allocating pair\n")
  (let ([ptr (alloc 3)])    
    (heap-set! ptr 'pair)
    (heap-set! (+ ptr 1) hd)
    (heap-set! (+ ptr 2) tl)
    ptr))

;; functions written by Jesse Sullivan

;; Allocates free space of size n on the heap and returns a pointer to it
;; if it can't find free space it runs the garbage collector and tries again
;; otherwise it throws an error saying that the heap is out of memory
(define (alloc n)
  (when (<= n 0) (error 'alloc "Tried to alloc ~a number of bytes\n" n))
  (let ([ptr_to_mem (find-free-space 0 n)])
    (cond [(and (<= 0 ptr_to_mem)
                (> (heap-size)　ptr_to_mem)) ptr_to_mem]  ; was able to find spot in mem on first try
          [else (collect-garbage) ; otherwise run the garbage collector
                (set! ptr_to_mem (find-free-space 0 n)) ; try again
                (if (eq? ptr_to_mem -1)
                    (error 'alloc "Unable to allocate memory") ; couldn't allocate memory
                    ptr_to_mem) ; could allocate mem after garbage collection
                ])))

;; Finds contiguous block of free memory for memory allocator (helper of alloc)
;; returns -1 if it is not able to find such a memory block
;; start is the ptr to the first block in memory
;; start + size is the pointer to the last block allocated if the mem is allocated
(define (find-free-space start size)
  ;(eprintf "In find-free-space, start is ~a\n" start)
  (cond [(eq? size 0) (error 'find-free-space "You can't allocate nothing on the heap")]
        [(and (eq? (+ start size) (heap-size))  ;; searched whole heap, couldn't allocate new memory
              (not (is-contiguous? start size))) -1]
        [(is-contiguous? start size) start] ;; was able to find contigous block of free mem, return ptr
        [else (find-free-space (+ start 1) size)] ;; else slide the window down by one and try again
        ))

;; helper function for find-free-space
;; returns true of the block between locations are all marked 'free
;; returns false otherwise
(define (is-contiguous? start size)
  (let ([block (for/fold ([blk (list)])
                         ([i (in-range size)])
                 (append blk `(,(heap-ref (+ start i)))))])
    ;(eprintf "block is ~a\n" block)
    (andmap (lambda (x) (eq? 'free x)) block)))

;; function to run garbage collector
;; uses the stop and copy algorithm for garbage collection
(define (collect-garbage)
  (printf "Collecting garbage\n"))
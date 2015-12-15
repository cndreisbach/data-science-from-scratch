#lang racket

(require test-engine/racket-tests)

;; These functions are available in math/statistics. Implementing for practice.

(define (sum xs)
  (foldl + 0 xs))

(define (mean xs)
  (/ (sum xs) (length xs)))

(define (median xs)
  (define len (length xs))
  (define sort-xs (sort xs <))
  (if (odd? len)
      (list-ref sort-xs (- (/ (+ len 1) 2) 1))
      (mean (list
             (list-ref sort-xs (- (/ len 2) 1))
             (list-ref sort-xs (/ len 2))))))

(define (percentile xs p)
  (define len (length xs))
  (define sort-xs (sort xs <))
  (define scorenum (* len p))
  (cond
    [(integer? scorenum)
     (mean (list (list-ref sort-xs (inexact->exact (sub1 scorenum)))
                 (list-ref sort-xs (inexact->exact scorenum))))]
    [else
     (list-ref sort-xs (inexact->exact (sub1 (ceiling scorenum))))]))

(define (mode xs)
  (define counter (make-hash))
  (for [(x xs)]
    (hash-update! counter x add1 0))
  (define max-count (apply max (hash-values counter)))
  (filter (lambda (x) (= max-count
                         (hash-ref counter x)))
          (hash-keys counter)))

(define (data-range xs)
  (- (apply max xs)
     (apply min xs)))

(define (de-mean xs)
  (define x-bar (mean xs))
  (map (lambda (x) (- x x-bar)) xs))

(define (sum-of-squares xs)
  (sum (map sqr xs)))

(define (variance xs)
  (/ (sum-of-squares (de-mean xs))
     (sub1 (length xs))))

(define (std-dev xs)
  (sqrt (variance xs)))

(define (interquartile-range xs)
  (- (percentile xs 0.75)
     (percentile xs 0.25)))


(define testnums '(43 54 56 61 62 66 68 69 69 70 71 72 77 78 79 85 87 88 89 93 95 96 98 99 99))
(check-expect (sum '(1 2 3 4)) 10)
(check-expect (mean '(1 2 3 4)) 5/2)
(check-expect (median '(9 4 -1)) 4)
(check-expect (median '(9 4 -1 7)) 11/2)
(check-expect (percentile testnums 0.9) 98)
(check-expect (percentile testnums 0.2) 64)
(check-expect (percentile testnums 0.5) (median testnums))
(check-expect (sort (mode testnums) <) '(69 99))
(check-expect (data-range testnums) 56)
(check-expect (de-mean '(1 3 5 7)) '(-3 -1 1 3))
(check-expect (sum-of-squares '(1 3 5 7)) 84)
(check-expect (variance '(1 3 5 7)) 20/3)
(test)
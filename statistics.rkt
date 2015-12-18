#lang racket

(require test-engine/racket-tests)
(require "vectors.rkt")

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

(define (covariance xs ys)
  (/ (dot (de-mean xs) (de-mean ys)) (sub1 (length xs))))

(define (correlation xs ys)
  (define std-dev-x (std-dev xs))
  (define std-dev-y (std-dev ys))
  (if (and (positive? std-dev-x) (positive? std-dev-y))
      (/ (covariance xs ys) std-dev-x std-dev-y)
      0))

(define (remove-outliers xs)
  (define top (+ (percentile xs 0.75) (interquartile-range xs)))
  (define bottom (- (percentile xs 0.25) (interquartile-range xs)))
  (filter (lambda (x) (and (>= x bottom) (<= x top))) xs))

(provide sum
         mean
         median
         percentile
         mode
         data-range
         de-mean
         variance
         std-dev
         interquartile-range
         sum-of-squares
         correlation
         remove-outliers)


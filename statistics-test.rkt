#lang racket/base
 
(require rackunit
         "statistics.rkt")


(define testnums '(43 54 56 61 62 66 68 69 69 70 71 72 77 78 79 85 87 88 89 93 95 96 98 99 99))
(check-equal? (sum '(1 2 3 4)) 10)
(check-equal? (mean '(1 2 3 4)) 5/2)
(check-equal? (median '(9 4 -1)) 4)
(check-equal? (median '(9 4 -1 7)) 11/2)
(check-equal? (percentile testnums 0.9) 98)
(check-equal? (percentile testnums 0.2) 64)
(check-equal? (percentile testnums 0.5) (median testnums))
(check-equal? (sort (mode testnums) <) '(69 99))
(check-equal? (data-range testnums) 56)
(check-equal? (de-mean '(1 3 5 7)) '(-3 -1 1 3))
(check-equal? (sum-of-squares '(1 3 5 7)) 84)
(check-equal? (variance '(1 3 5 7)) 20/3)
(check-= (correlation testnums testnums) 1 0.0)
(check-equal? (remove-outliers '(0 0 10 12 15 16 16 17 22 45))
              '(10 12 15 16 16 17 22))
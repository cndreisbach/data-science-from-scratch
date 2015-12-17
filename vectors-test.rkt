#lang racket

(require rackunit
         "vectors.rkt")

(check-equal? '(4 6) (vector-add '(1 2) '(3 4)))
(check-exn exn:fail? (lambda () (vector-add '(1 2) '(3 4 5))))
(check-equal? '(-2 -2) (vector-sub '(1 2) '(3 4)))
(check-equal? '(2 8) (vector-sum '((1 2) (3 4) (-2 2))))
(check-equal? '(1 3) (vector-mean '((1 2) (3 4) (-1 3))))
(check-equal? '(3 9 15) (vector-mul '(1 3 5) 3))
(check-equal? 17 (dot '(7 5) '(1 2)))
(check-equal? 5 (magnitude '(3 4)))
(check-equal? 0 (distance '(3 4) '(3 4)))
(check-equal? 1 (distance '(1 1) '(2 1)))
(check-equal? 5 (distance '(1 1) '(4 5)))
(check-equal? 5 (distance '(4 5) '(1 1)))
#lang racket

(require test-engine/racket-tests)

(define (matrix-shape A)
  (if (empty? A)
      '(0 0)
      (list (length A) (length (first A)))))

(define (matrix-row A n)
  (cond
    [(empty? A) (error "Row does not exist")]
    [(= n 0) (first A)]    
    [(> n 0) (matrix-row (rest A) (sub1 n))]
    [else (error "Invalid row number")]))

(define (matrix-col A n)
  (if (empty? A)
      '()
      (cons (list-ref (first A) n) (matrix-col (rest A) n))))

(define (make-matrix rows cols fn)
  (define (make-row row-n)
    (for/list [(col-n (range cols))]
      (fn row-n col-n)))
  (for/list [(row-n (range rows))]
    (make-row row-n)))

(check-expect '(2 3) (matrix-shape '((1 0 1)
                                     (0 1 0))))
(check-expect '(0 1 0) (matrix-row '((1 0 1)
                                     (0 1 0)) 1))
(check-expect '(0 1) (matrix-col '((1 0 1)
                                   (0 1 0)) 1))
(check-expect '((1 0 0)
                (0 1 0)
                (0 0 1)) (make-matrix 3 3 (lambda (x y) (if (= x y) 1 0))))
(check-expect '((0 1 2)
                (1 2 3)
                (2 3 4)) (make-matrix 3 3 +))

(test)
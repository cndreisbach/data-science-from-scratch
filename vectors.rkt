#lang racket

(require test-engine/racket-tests)

(define (vector-negative v)
  (if (empty? v)
      '()
      (cons (- (first v)) (vector-negative (rest v)))))

(define (vector-add v w)
  (cond
    [(and (empty? v) (empty? w)) '()]
    [(or (empty? v) (empty? w)) (error "Vectors must be of same length.")]
    [else (cons (+ (first v) (first w)) (vector-add (rest v) (rest w)))]))

(define (vector-sub v w)
  (vector-add v (vector-negative w)))

(define (vector-mul vec c)
  (if (empty? vec)
      '()
      (cons (* (first vec) c) (vector-mul (rest vec) c))))

(define (vector-sum vecs)
 (cond
   [(empty? vecs) (error "Cannot take the sum of 0 vectors")]
   [(= (length vecs) 1) (first vecs)]
   [else (foldl vector-add (first vecs) (rest vecs))]))

(define (vector-mean vecs)
  (vector-mul (vector-sum vecs) (/ 1 (length vecs))))

(define (dot v w)
  (cond
    [(and (empty? v) (empty? w)) 0]
    [(or (empty? v) (empty? w)) (error "Vectors must be of same length.")]
    [else (+ (* (first v) (first w)) (dot (rest v) (rest w)))]))

(define (magnitude vec)
  (sqrt (dot vec vec)))

(define (distance v w)
  (magnitude (vector-sub v w)))

(check-expect '(4 6) (vector-add '(1 2) '(3 4)))
(check-error (vector-add '(1 2) '(3 4 5)))
(check-expect '(-2 -2) (vector-sub '(1 2) '(3 4)))
(check-expect '(2 8) (vector-sum '((1 2) (3 4) (-2 2))))
(check-expect '(1 3) (vector-mean '((1 2) (3 4) (-1 3))))
(check-expect '(3 9 15) (vector-mul '(1 3 5) 3))
(check-expect 17 (dot '(7 5) '(1 2)))
(check-expect 5 (magnitude '(3 4)))
(check-expect 0 (distance '(3 4) '(3 4)))
(check-expect 1 (distance '(1 1) '(2 1)))
(check-expect 5 (distance '(1 1) '(4 5)))
(check-expect 5 (distance '(4 5) '(1 1)))
(test)
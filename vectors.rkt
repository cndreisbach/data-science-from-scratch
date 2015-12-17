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

(provide vector-add
         vector-sub
         vector-mul
         vector-sum
         vector-mean
         dot
         magnitude
         distance)

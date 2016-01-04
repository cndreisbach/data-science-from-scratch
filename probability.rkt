#lang racket

(require plot)
(require math/special-functions)

(define (normal-pdf mu sigma)
  (define sqrt-two-pi (sqrt (* 2 pi)))
  (lambda (x)
    (exp (/ (/ (- (expt (- x mu) 2)) 2 (expt sigma 2)) (* sqrt-two-pi sigma)))))

(define (normal-cdf mu sigma)
  (lambda (x)
    (/ (+ 1 (erf (/ (- x mu) (sqrt 2) sigma))) 2)))

(define (plot-pdf)
  (plot (function (normal-pdf 0 1) -5 5)))

(define (bernoulli-trial p)
  (if (< (random) p)
      1
      0))

(define (binomial n p)
  (foldl + 0 (for/list ([_ (range n)])
               (bernoulli-trial p))))

;def make_hist(p, n, num_points):
;data = [binomial(n, p) for _ in range(num_points)]
;    # use a bar chart to show the actual binomial samples
;histogram = Counter(data)
;plt.bar([x - 0.4 for x in histogram.keys()],
;[v / num_points for v in histogram.values()], 0.8,
;color='0.75')
;mu=p*n
;sigma = math.sqrt(n * p * (1 - p))
;    # use a line chart to show the normal approximation
;    xs = range(min(data), max(data) + 1)
;    ys = [normal_cdf(i + 0.5, mu, sigma) - normal_cdf(i - 0.5, mu, sigma)
;foriinxs] plt.plot(xs,ys)
;    plt.title("Binomial Distribution vs. Normal Approximation")
;    plt.show()

(define (counter data)
  (foldl (lambda (key acc)
           (hash-update acc key add1 0)) (hash) data))

(define (make-x-histo data)
  (define xhash (counter data))
  (define num-points (length data))
  (map (lambda (x)
         (vector (ivl (- (car x) 0.4) (+ (car x) 0.4))
                 (ivl 0 (/ (cdr x) num-points))))
       (hash->list xhash)))

(define (make-lines mu sigma data)
  (define xs (range (apply min data) (add1 (apply max data))))
  (define cdf (normal-cdf mu sigma))
  (define ys (map (lambda (x)
                    (- (cdf (+ x 0.5)) (cdf (- x 0.5)))) xs))
  (for/list ([x xs]
             [y ys])
    (vector x y)))

(define (make-hist p n num-points)
  (define data (for/list ([_ (range num-points)])
                 (binomial n p)))
  (define mu (* p n))
  (define sigma (sqrt (* n p (- 1 p))))
  (plot (list (rectangles (make-x-histo data))
              (lines (make-lines mu sigma data)))))

(make-hist 0.75 100 10000)
#lang racket

(define (sqrt x)
    (define (abs x)
      (if (< x 0) (- x) x))
  
    (define (square x)
      (* x x))

    (define (sqrt-iter guess x)
      (if (goodenough? guess x) guess
        (sqrt-iter (improve guess x) x)))
  
    (define (goodenough? guess x)
      (< (abs (- (square guess) x)) 0.01))

    (define (improve guess x)
      (average (/ x guess) guess))

    (define (average a b)
      (/ (+ a b) 2))
  
  (sqrt-iter 1.0 x))

(sqrt 13)






(sqrt 13)









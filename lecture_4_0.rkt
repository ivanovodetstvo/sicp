#lang racket

; f(a) < 0 < f(b)
; x = average(a,b)
; f(x) > 0 --> a,x
; f(x) < 0 --> x,b


(define (search f neg-p pos-p)
  (define (average x y) (/ (+ x y) 2))

  (define (close-enough? x y) (< (abs (- x y)) 0.001))

  (define (positive? value) (> value 0))

  (define (negative? value) (< value 0))

  (define (error err a b) (err))

  (define (half-interval-method f a b)
    (let ( (a-value (f a)) (b-value (f b)) )
      (cond
        ((and (negative? a-value) (positive? b-value)) (search f a b))
        ((and (negative? b-value) (positive? a-value)) (search f b a))
        ((error "Error! " a b))
      )
    )
  )
  
  (let ( (mid-p (average neg-p pos-p)) )
    (if (close-enough? neg-p pos-p) mid-p
      (let ( (test-value (f mid-p)) )
        (cond
          ((positive? test-value) (half-interval-method f neg-p mid-p))
          ((negative? test-value) (half-interval-method f mid-p pos-p))
          ((mid-p))
        )
      )
    )
  )
)

(search (lambda (x) (- (* x x x) (* 2 x) 3)) 1.0 2.0)
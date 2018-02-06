#lang racket

; finding fixed point f(x)=x ... f(x), f(f(x)), f(f(f(x)))...



(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))

  (define (next x) (x))

  (define (try guess)
    (let ( (next (f guess)) )
      (if (close-enough? guess next) next
          (try next))
    )
  )

  (try first-guess)
)

(fixed-point cos 1.0)
(fixed-point (lambda (y) (+ (sin y) (cos y) )) 1.0)

; y^2=x => y=x/y --> does not converge
; y1, y2=x/y1, y3=x/y2=y1
; 1/2(y + x/y) --> average

; усреднение последовательных приближений -> торможение усреднением / average damping
(define (sqrt x)
  (define (average x y) (/ (+ x y) 2))
  
  (fixed-point (lambda (y) (average y (/ x y))) 1.0)
)

(define (average-damp f)
  (define (average x y) (/ (+ x y) 2))
 
  (lambda (x) (average x (f x)))
)

(define (square x) (* x x))

((average-damp square)10)









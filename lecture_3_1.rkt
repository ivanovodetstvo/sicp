#lang racket

(define (sum-int a b)
  (if (> a b) 0
      (+ a (sum-int (+ a 1) b))))

(sum-int 3 10)

(define (sum-cubes a b)
  (define (cube a)
    (* a a a))
  
  (if (> a b) 0
      (+ (cube a) (sum-cubes (+ a 1) b))))

(sum-cubes 3 10)

; pi-sum = pi/8
(define (pi-sum a b)
  (if (> a b) 0
      (+ (/ 1 (* a (+ a 2))) (pi-sum (+ 4 a) b))))

(pi-sum 1 10)


; abstraction of summation of sequences

(define (sum term a next b)
  (if (> a b) 0
      (+ (term a) (sum term (next a) next b))))

(define (identity x) x)

(define (inc x) (+ x 1))

(define (sum-i a b)
  (sum identity a inc b))

(sum-i 3 10)

(define (cubes x)
  (* x x x))

(define (sum-c a b)
  (sum cubes a inc b))

(sum-c 3 10)

(define (reformer-pi x)
  (/ 1 (* x (+ x 2))))

(define (inc-pi x)
  (+ x 4))

(define (sum-pi a b)
  (sum reformer-pi a inc-pi b))

(sum-pi 1 10)

; with lambda
(define (sum-pi-l a b)
  (sum (lambda (x) (/ 1 (* x (+ x 2))))
       a
       (lambda (x) (+ x 4))
       b))

(sum-pi-l 1 10)


; f(x,y) = x(1+xy)^2 + y(1-y) + (1+xy)(1-y)
; a = (1+xy), b = (1-y) 
; f(x,y) = xa^2 + yb + ab

(define (f x y)
  (define (square x) (* x x))
  
  (define (f-helper a b)
    (+ (* x (square a)) (* y b) (* a b) ))

  (f-helper (+ 1 (* x y)) (- 1 y)))

(f 3 3)

; with lambda
; ? arguments for lambda --> let
(define (f-l x y)
  (define (square x) (* x x))

  (
   let ( (a (+ 1 (* x y))) (b (- 1 y)) )
    (+ (* x (square a)) (* y b) (* a b) )
  )
)


(f-l 3 3)











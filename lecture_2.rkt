#lang racket
; linear recursive process (recursive procedure and recursive process)
; vs
; linear iterative process (recursive procedure and iterative process) -> its syntax construct recursive, not the process itself)
; effective memory usage here (in scheme) / not needed in cycles --> tail recursion --> effective procedures without cycles 

(define (fact-r n)
  (if (= n 1) 1 (* n (fact-r (- n 1)))))

(define (fact-i n)
  (define (fact-iter x counter n)
    (if (> counter n) x
        (fact-iter (* x counter) (+ counter 1) n)))
  (fact-iter 1 1 n))

(fact-r 7)
(fact-i 13)

(define (fib-r n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        ((+ (fib-r (- n 1)) (fib-r (- n 2))))))

(define (fib-i n)
  (define (fib-iter a b counter)
    (if (= counter 0) b
        (fib-iter (+ a b) a (- counter 1))))
  (fib-iter 1 0 n))
  
(fib-r 2)
(fib-i 2)


; b^n, b = b*b^n-1 , b^0=1
; O(n), memory O(n)
(define (expt b n)
  (if (= n 0) 1
      (* b (expt b (- n 1)))))


; O(n), memory O(1)
(define (expt-i b n)
  (define (expt-iter b counter prod)
    (if (= counter 0) prod
        (expt-iter b (- counter 1) (* prod b))))
  (expt-iter b n 1))

; O(logn), memory O(logn)
(define (fast-expt b n)
  
  (define (square x)
    (* x x))
  
  (define (even? x)
    (= (modulo x 2) 0))
  
  (cond ((= n 0) 1)
        ((even? n)
         (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(expt 2 3)
(expt-i 2 3)
(fast-expt 2 3)



  

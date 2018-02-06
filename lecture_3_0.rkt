#lang racket
; Euclidean algorithm and Lame's theorem
; gcd(a,b) --> gcd(a,b) = gcd(b,r), r = a/b
; n >= Fib(k), n = min(a,b), k = euclidean algo efforts
; F^k/sqrt(5) --> ... --> O(log n)

; O(log n)
(define (gcd a b)
  (if (= b 0) a
      (gcd b (remainder a b))))

(gcd 206 40)

; prime number --> if not prime: divisor <= sqrt(n)
; 0(sqrt(n))
(define (smallest-divisor n)
  (define (find-divisor n test-divisor)
    (define (square x)
      (* x x))
    
    (define (divides? n test-divisor)
      (= (remainder n test-divisor) 0))
    
    (cond ((> (square test-divisor) n) n)
          ((divides? n test-divisor) test-divisor)
          ((find-divisor n (+ test-divisor 1)))))

  (find-divisor n 2))

(define (prime? n)
  (= n (smallest-divisor n)))

(prime? 167)
(prime? 168)

; Fermat's little theorem
; with probability as much as possible, but .. (Carmichael numbers; 255 nums < 100m, e.g. 561/1105 ...) --> probabilistic method
; n = prime, a = arbitrary int, a < n, --> a^n = a mod n;

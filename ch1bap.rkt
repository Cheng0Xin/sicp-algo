#lang racket

(define (square x) (* x x))

(define (sum-of-squares x y) 
  (+ (square x) (square y)))

(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))

(define (abs_ x)
  (cond ((> x 0) x) 
        (else (- x))))

(define (abs__ x) 
  (if (< x 0) (- x) x))

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

(define (sqrt x) 
  (define (sqrt-iter guess)
    (if (good-enough guess) 
        guess 
        (sqrt-iter (improve guess))))
  (define (good-enough guess)
    (< (abs (- (/ (square guess) x) 1)) 0.0001))
  (define (improve guess)
    (average (/ x guess) guess))
  (define (average y)
    (/ (+ x y) 2))
  (sqrt-iter (/ x 2.0)))

(define (cube x) 
  (define (cube-iter guess) 
    (if (good-enough guess) guess
        (cube-iter (improve guess))))
  (define (good-enough y) 
    (< (abs (- 1 (/ (* y y y) x))) 0.0001))
  (define (improve y)
    (/ (+ (/ x (square y)) (* 2 y)) 3))
  (cube-iter 1.0))


(define (factorial n)
  (define (factorial-iter n sum) 
    (if (= n 1) sum
        (factorial-iter (- n 1) (* sum n))))
  (factorial-iter n 1))

(define (fibonacci n)
  (define (iter p0 p1 n)
    (cond ((= n 0) p0)
          ((= n 1) p1)
          (else (iter p1 (+ p1 p0) (- n 1)))))
  (iter 0 1 n))

(define (count-change amount) 
  (define (cc amount kinds-of-coins) 
    (cond ((= amount 0) 1)
          ((or (< amount 0) (= kinds-of-coins 0)) 0)
          (else (+ (cc amount (- kinds-of-coins 1)) 
                   (cc (- amount 
                          (first-denomination kinds-of-coins)) 
                       kinds-of-coins)))))

  (define (first-denomination kinds-of-coins)
    (cond ((= kinds-of-coins 1) 1)
          ((= kinds-of-coins 2) 5)
          ((= kinds-of-coins 3) 10)
          ((= kinds-of-coins 4) 25)
          ((= kinds-of-coins 5) 50)))
  (cc amount 5))

(define (exerf n)
  (if (< n 3) n 
      (+ (exerf (- n 1)) 
         (* 2 (exerf (- n 2)))
         (* 3 (exerf (- n 3))))))

(define (sine x)
  (define (cu x) (* x x x))
  (define (p x) (- (* 3.0 x) (* 4.0 (cu x))))
  (if (< x 0.1)
      x
      (p (sine (/ x 3.0)))))

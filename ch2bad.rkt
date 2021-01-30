;ch2bad.rkt

(define mul *)
(define add +)
(define sub -)

(define (make-rat x y) 
  (let ((g (gcd x y)))
  (cons (/ x g) (/ y g))))
(define denom cdr)
(define numer car)

(define (add-rat x y)
  (make-rat (add (mul (denom x) (numer y)) 
                 (mul (denom y) (numer x)))
            (mul (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (mul (numer x) (numer y))
            (mul (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (sub (mul (numer x) (denom y))
                 (mul (numer y) (denom x)))
            (mul (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (mul (denom y) (numer x))
            (mul (denom x) (numer y))))

(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline))

; test
(print-rat (add-rat (make-rat 3 5) (make-rat 1 2)))
(print-rat (mul-rat (make-rat 1 2) (make-rat 3 4)))
(print-rat (sub-rat (make-rat 1 2) (make-rat 3 4)))
(print-rat (div-rat (make-rat 1 2) (make-rat 3 4)))

(define (test-let)
  (let ((a 1)
        (b 2)
        (c 10))
    (+ a b c)))

(define (test-add a x y)
  (a x y))

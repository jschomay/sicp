#lang racket


(define (test name pred)
  (if pred
    (string-append name " passed!")
    (string-append name " failed")))

(define (run name code)
  (display (string-append "\n\n" name " results:\n"))
  code)


;;;; Ch 1 ;;;;

; Ex 1.3 largest squares

(define (square a) (* a a))

(define (e1.3 a b c)
  (cond
    ((and (< a b) (< a c)) (+ (square b) (square c)))
    ((and (< b a) (< b c)) (+ (square a) (square c)))
    (else (+ (square a) (square b)))))

(test "e1.3"
      (and
        (= (e1.3 1 2 3) 13)
        (= (e1.3 3 2 1) 13)
        (= (e1.3 5 2 3) 34)))


; Ex 1.6/7 square roots

; generalized by providing the improve-fn
(define (roots improve-fn guess prev-guess x)
  (if (good-enough? guess prev-guess x)
      guess
      (roots improve-fn (improve-fn guess x)
                 guess
                 x)))

(define (sqrt-iter improve-fn guess prev-guess x)
  (roots improve-fn guess prev-guess x))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess prev-guess x)
  (< (/ (abs (- guess prev-guess)) guess)
     0.001))
; the old way:
;  (< (abs (- (square guess) x))
;     0.001)

(define (improve guess x)
    (average guess (/ x guess)))

(define (sqrt x)
  (sqrt-iter improve 1.0 0 x))

(define (bad-if pred then else_)
  (cond (pred then)
        (else else_)))
; runs for ever, don't do this

(run "e1.6/7"
     (map (lambda (n) (list n '-> (sqrt n)))
          '(2 9 25 100 10000 123)))

#|
'((2 -> 1.4142156862745097)
  (9 -> 3.00009155413138)
  (25 -> 5.000023178253949)
  (100 -> 10.000000000139897)
  (10000 -> 100.00000025490743)
  (123 -> 11.090536508377188))

vs.

'((2 -> 1.4142135623746899)
  (9 -> 3.000000001396984)
  (25 -> 5.000000000053722)
  (100 -> 10.000000000139897)
  (10000 -> 100.00000025490743)
  (123 -> 11.090536508377188))
 |#

; Ex 1.8 cubed root

(define (improve-cubed guess x)
  (/ (+ (/ x (square guess))
        (* guess 2))
     3))

(define (cuberoot x)
  (roots improve-cubed 1.0 0 x))

(run "e.1.8"
  (map (lambda (x) (list x (cuberoot x))) '(1 27 125 8 99 1000 100000)))

#|
'((1 1.0)
  (27 3.0000005410641766)
  (125 5.000000000287929)
  (8 2.000000000012062)
  (99 4.626066084248356)
  (1000 10.000000145265767)
  (100000 46.41588833734411))
|#



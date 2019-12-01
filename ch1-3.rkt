#lang racket

; Ex 1.29

(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

(define (cube a) (* a a a))
(define (inc a) (+ a 1))

(define (even? n) (= (remainder n 2) 0))
(define (odd? n) (not (even? n)))

(define (simpsons-rule f a b n)
  (define h (/ (- b a) n))
  (define (y k) (f (+ a (* k h))))
  (define (term x)
    (cond
      ((= x n) (y x))
      ((= x 0) (y x))
      ((odd? x) (* 4 (y x)))
      (else (* 2 (y x)))))
  (* (/ h 3)
     (sum term a inc n)))

(simpsons-rule cube 0 1.0 1000)

; (simpsons-rule cube 0 1.0 1000))
; 0.2500000000000003


; Ex 1.30

(define (sum-iter term a next b)
  (define (iter a res)
    (if (> a b)
      res
      (iter (next a) (+ (term a) res))))
  (iter a 0))


(sum cube 5 inc 10)
(sum-iter cube 5 inc 10)


; Ex 1.31

(define (prod term a next b)
  (if (> a b)
    1
    (* (term a)
       (prod term (next a) next b))))

(* 1 2 3 4 5 6 7 8 9 10)
(prod identity 1 inc 10)
(+ 1 2 3 4 5 6 7 8 9 10)
(sum identity 1 inc 10)
(define (factorial n) (prod identity 1 inc n))
(define (factorial-iter n) (prod-iter identity 1 inc n))
(factorial 10)
(factorial-iter 10)

(define (prod-iter term a next b)
  (define (iter a res)
    (if (> a b)
      res
      (iter (next a) (* (term a) res))))
  (iter a 1))


; Ex 1.32

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a res)
    (if (> a b)
      res
      (iter (next a) (combiner (term a) res))))
  (iter a null-value))

(define (accumulate combiner null-value term a next b)
  (if (> a b)
    null-value
    (combiner (term a)
       (accumulate combiner null-value term (next a) next b))))

;M stands for "M"onoid
(define (prodM term a next b) (accumulate * 1 term a next b))
(define (prodM-iter term a next b) (accumulate-iter * 1 term a next b))
(=
  (prod identity 1 inc 10)
  (prod-iter identity 1 inc 10)
  (prodM identity 1 inc 10)
  (prodM-iter identity 1 inc 10)
  3628800)


; Ex 1.33
(define (filtered-accumulate filterer combiner null-value term a next b)
  (define (iter a res)
    (cond
      ((> a b) res)
      ((filterer a) (iter (next a) (combiner (term a) res)))
      (else (iter (next a) res))))
  (iter a null-value))


(+ 2 4 6 8 10)
(filtered-accumulate even? + 0 identity 1 inc 10)

; Ex. 1.36

(define (average a b) (/ (+ a b) 2))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess))

(fixed-point (lambda (y) (/ (log 1000) (log y))) 4.0)
; 4.0
; 4.9828921423310435
; 4.301189432497896
; 4.734933901055578
; 4.442378437719526
; 4.632377941509958
; 4.505830646780212
; 4.588735606875766
; 4.533824356566501
; 4.56993352418142
; 4.546075272637246
; 4.561789745175654
; 4.55141783665413
; 4.5582542120702625
; 4.553744140202578
; 4.556717747893265
; 4.554756404545319
; 4.5560497413912975
; 4.5551967522618035
; 4.555759257615811
; 4.555388284933278
; 4.555632929754932
; 4.555471588998784
; 4.555577989320218
; 4.555507819903776
; 4.555554095154945
; 4.555523577416557
; 4.555543703263474
; 4.555530430629037
; 4.555539183677709

(fixed-point (lambda (y) (average y (/ (log 1000) (log y)))) 4.0)
; 4.0
; 4.491446071165521
; 4.544974650975552
; 4.553746974742814
; 4.555231425802502
; 4.555483906560562
; 4.5555268862194875
; 4.5555342036887705


; Ex 1.41


(define (double f)
  (lambda (x) (f (f x))))

((double inc) 1)
; 3

(((double (double double)) inc) 5)
; ~14~ ??
; 21

; Ex 1.42

(define (compose f g)
  (lambda (x) (f (g x))))

(define (square x) (* x x))

(= ((compose square inc) 6) 49)

; Ex 1.43

(define (repeated f n)
  (define (rep acc n)
    (cond
      ((< n 1) identity)
      ((= n 1) acc)
      (else (rep (compose f acc) (- n 1)))))
  (rep f n))

(define (repeated-itr f n)
  (cond
    ((< n 1) identity)
    ((= n 1) f)
    (else (compose f (repeated-itr f (- n 1))))))

(= ((repeated square 2) 5) 625) ; (square (square 5))
(= ((repeated square 3) 5) 390625) ; (square (square (square 5)))

; if n < 1 just return the argument (ie. apply it 0 times)
(= ((repeated inc 0) 99) 99)
(= ((repeated-itr inc 0) 99) 99)

; Ex 1.44

(define (smooth f)
  (let ((dx 0.01))
    (lambda (x)
      (/ (+ (f x)
            (f (+ x dx))
            (f (- x dx)))
         3))))

; (define (n-fold-smooth f n)
;  ((repeated smooth n) f))

(define (n-fold f n)
  (repeated f n))

(define (n-fold-smooth f n)
  ((n-fold smooth n) f))

(sin 1)
((smooth sin) 1)

(=
  ((smooth (smooth sin)) 1)
  (((repeated smooth 2) sin) 1)
  ((n-fold-smooth sin 2) 1))

(=
  ((smooth (smooth (smooth sin))) 1)
  (((repeated smooth 3) sin) 1)
  ((n-fold-smooth sin 3) 1))


; Ex 1.45

(define (root-2 x)
  (fixed-point (lambda (y) (average y (/ x y))) 4.0))
(root-2 9)

(define (root-3 x)
  (fixed-point (lambda (y) (average y (/ x (* y y)))) 4.0))
(root-3 27)

(define (avg-damp f)
  (lambda (y) (average y (f y))))

(define (avg-damp-n f n)
  ((repeated avg-damp n) f))

(define (root-3-b x)
  (fixed-point (avg-damp (lambda (y) (/ x (* y y)))) 4.0))
(root-3-b 27)

(define (root-4 x)
  (fixed-point (avg-damp-n (lambda (y) (/ x (* y y y))) 2) 4.0))
(root-4 81)

(define (root-5 x)
  (fixed-point (avg-damp-n (lambda (y) (/ x (* y y y y))) 2) 4.0))
(root-5 243)

;

(define (pow e n)
  ((repeated (lambda (x) (* x e)) n) 1))

(define (root-test n damp-count)
   (let ((x (pow 3 n)))
     (fixed-point (avg-damp-n (lambda (y) (/ x (pow y (- n 1)))) damp-count) 4.0)))

(root-test 1 1)
(root-test 2 1)
(root-test 3 1)
; 2^2
(root-test 4 2)
(root-test 5 2)
(root-test 6 2)
(root-test 7 2)
; 2^3
(root-test 8 3)
(root-test 9 3)
(root-test 10 3)
(root-test 11 3)
(root-test 12 3)
(root-test 13 3)
(root-test 14 3)
(root-test 15 3)
; 2^4
(root-test 16 4)
; ...
(root-test 31 4)
; 2^5
(root-test 32 5)
; ...
(root-test 63 5)
(root-test 64 6)

(define (root-nth n x)
  (let ((fn (lambda (y) (/ x (pow y (- n 1)))))
        (guess 4.0)
        (damp-count (floor (log n 2))))
    (fixed-point (avg-damp-n fn damp-count) 4.0)))

(root-nth 3 27)
(root-nth 15 (pow 3 15))
(root-nth 16 (pow 3 16))
(root-nth 17 (pow 3 17))
(root-nth 65 (pow 3 65))
; takes a long ime, but gets there!
; (root-nth 100 (pow 10 100))

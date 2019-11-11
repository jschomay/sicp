
(define (run name fn args)
  (display (string-append "\n\n" name " results:\n"))
  (string-join
    ( map
      (lambda (x)
        (display (list fn x))
        (display " = ")
        (display (fn x))
        (display "\n")
        "-")
      args)))


(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))

;~ n * 2
(define (f n) (A 0 n))
;~ 2 ^ n
(define (g n) (A 1 n))
;~ 2 ^ (2 ^ n)) ???
(define (h n) (A 2 n))

#|
making change:

5 p,n = 2
0n 5p
1n 0p

10 p,n = 3
# 10 p (1)
# 5 p,n (2)
---
0n 10p
1n 5p
2n 0p

15 p,n = 4
# 15 p (1)
# 10 n,p (3)
---
0n 15p
1n 10p
2n 5p
3n 0p

20 p,n = 5
# 20 p (1)
# 15 p,n (4)
---
0n 20p
1n 15p
2n 10p
3n 5p
4n 0p

----------

10 p,n,d = 4
# 10 p,n (3)
# 0 p,n,d (1)
---
0d 0n 10p
0d 1n 5p
0d 2n 0p
1d 0n 0p

20 p,n,d = 9
# 20 p,n (5)
# 10 p,n,d (4)
---
0d 0n 20p
0d 1n 15p
0d 2n 10p
0d 3n 5p
0d 4n 0p
1d 0n 10p
1d 1n 5p
1d 2n 0p
2d 0n 0p


8 p,n = 2
# 8 p (1)
# 3 p,n (1)

13 p,n = 3
# 13 p (1)
# 8 p,n (2)

18 p,n = 4
# 18 p (1)
# 13 p,n (3)

8 p,n,d = 2
# 8 p,n (2)

18 p,n,d = 6
# 18 p,n (4)
# 8 p,n,d (3)
---
0d 0n 18p
0d 1n 13p
0d 2n 8p
0d 3n 3p
1d 0n 8p
1d 1n 3p
|#

(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount (- kinds-of-coins 1))
                 (cc (- amount (first-denomination kinds-of-coins))
                     kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

(run "count-change" count-change `(5 10 18 20 100))
; count-change results:
; (#<procedure:count-change> 5) = 2
; (#<procedure:count-change> 10) = 4
; (#<procedure:count-change> 18) = 6
; (#<procedure:count-change> 20) = 9
; (#<procedure:count-change> 100) = 292

; Ex 1.11

(define (e1-11-rec n)
  (cond
    ((< n 3) n)
    (else (+
            (* 1 (e1-11-rec (- n 1)))
            (* 2 (e1-11-rec (- n 2)))
            (* 3 (e1-11-rec (- n 3)))))))

(run "Ex 1.11 recursive" e1-11-rec '(0 1 2 3 4 5 6 7 8 9 10 20))
; Ex 1.11 recursive results:
; (#<procedure:e1-11-rec> 0) = 0
; (#<procedure:e1-11-rec> 1) = 1
; (#<procedure:e1-11-rec> 2) = 2
; (#<procedure:e1-11-rec> 3) = 4
; (#<procedure:e1-11-rec> 4) = 11
; (#<procedure:e1-11-rec> 5) = 25
; (#<procedure:e1-11-rec> 6) = 59
; (#<procedure:e1-11-rec> 7) = 142
; (#<procedure:e1-11-rec> 8) = 335
; (#<procedure:e1-11-rec> 9) = 796
; (#<procedure:e1-11-rec> 10) = 1892
; (#<procedure:e1-11-rec> 20) = 10771211
; "- - - - - - - - - - - -"

; 0 | 0
; 1 | 1
; 2 | 2
; 3 | (* 3 0) + (* 2 1) + 2 = 4
; 4 | (* 3 1) + (* 2 2) + 4 = 11

(define (e1-11-iter n)
  (define (iter a b c count)
    (cond
      ((= count 0) c)
      ((= count 1) b)
      ((= count 2) a)
      (else (iter (+ (* 3 c) (* 2 b) a) a b (- count 1)))))
  (iter 2 1 0 n))

(run "Ex 1.11 iterative" e1-11-iter '(0 1 2 3 4 5 6 7 8 9 10 20))

; Ex 1.11 iterative results:
; (#<procedure:e1-11-iter> 0) = 0
; (#<procedure:e1-11-iter> 1) = 1
; (#<procedure:e1-11-iter> 2) = 2
; (#<procedure:e1-11-iter> 3) = 4
; (#<procedure:e1-11-iter> 4) = 11
; (#<procedure:e1-11-iter> 5) = 25
; (#<procedure:e1-11-iter> 6) = 59
; (#<procedure:e1-11-iter> 7) = 142
; (#<procedure:e1-11-iter> 8) = 335
; (#<procedure:e1-11-iter> 9) = 796
; (#<procedure:e1-11-iter> 10) = 1892
; (#<procedure:e1-11-iter> 20) = 10771211
; "- - - - - - - - - - - -"


; Ex 1.12

; n | 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15
; p | 1 1 1 1 2 1 1 3 3 1  1  4  6  4  1

; ?????????????
; not sure how to implement interior functions :(
(define (pascals-triangle n)
  (define (is-edge? n)
    ; member of n! or n! - 1 ????
    (n))
  (define (row-count n)
    ; ???
    (n))
  (define (above-l n)
    ; n - (row-count n)
    (n))
  (define (above-r n)
    ; n - (row-count n) - 1
    (n))
  (cond
    ((is-edge? n) 1)
    (else (+
            (pascals-triangle (above-l n))
            (pascals-triangle (above-r n))))))

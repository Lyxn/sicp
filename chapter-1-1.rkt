#lang racket

;----TextBook----
(define (square x) (* x x))
(define (cube x) (* x x x))

(define (mgcd a b)
  (if (= b 0)
      a
      (mgcd b (remainder a b))))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))
(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (carm? n)
  (define (try-it a)
    (= (expmod a n n) a))
  (define (carm-it a)
    (cond ((= a 1) true)
          ((try-it a) (carm-it (- a 1)))
          (else a)))
  (carm-it (- n 1)))

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))
(define (prime? n)
  (= n (smallest-divisor n)))

;----Excercise----
;Excercise 1.5
(define (p) (p))
(define (test x y)
  (if (= x 0)
      0
      y))
;(test 0 (p))

;Excercise 1.8
;squrae-root
(define (sqrt x)
  (define (imporve g) (/ (+ g (/ x g)) 2))
  (define (good-enough? guess) (< (abs (- (square guess) x)) 0.001))
  (define (iter g)
    (if (good-enough? g)
        g
        (iter (imporve g))
   ))
  (iter 1.0))
;cube-root
(define (cbrt x)
  (define (imporve g) (/ (+ (/ x (square g)) (* 2 g)) 3))
  (define (good-enough? guess) (< (abs (- (cube guess) x)) 0.001))
  (define (iter g)
    (if (good-enough? g)
        g
        (iter (imporve g))
   ))
  (iter 1.0))
(sqrt 64)
(cbrt 64)

;Exercise 1.16
(define (fast-expt b n)
  (expt-iter 1 b n))
(define (expt-iter a b n)
  (cond ((= n 0) a)
        ((even? n) (expt-iter a (* b b) (/ n 2)))
        (else (expt-iter (* a b) b (- n 1)))))
(fast-expt 2 6)

;Exercise 1.19
;fib
(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count) (fib-iter a
                                 b
                                 (+ (* p q 2) (* q q))
                                 (+ (* p p) (* q q))
                                 (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                                 (+ (* b p) (* a q))
                                 p
                                 q
                                 (- count 1)))
   ))

;Exercise 1.28
; nontrival square root 561, 188 67 
(define (sigexpmod base exp0 m)
  (define (nontrivial ret)
    (and (not (= ret 1))
         (not (= ret (- m 1)))
         (= (remainder (square ret) m) 1)))
  (define (em-it exp)
    (cond ((= exp 0) 1)
          ((even? exp)           
           (let ((ret (em-it (/ exp 2))))
             (if (nontrivial ret)
                 0
                 (remainder (square ret) m))))
          (else
           (remainder (* base (em-it (- exp 1)))
                    m))))
  (em-it exp0))

(define (miller-rabin-test n)
  (define (try-it a)
    (= (sigexpmod a (- n 1) n) 1))
  (try-it (+ 2 (random (- n 3)))))

(define (mr-prime? n times)
  (cond ((= times 0) true)
        ((miller-rabin-test n) (mr-prime? n (- times 1)))
        (else false)))

(mr-prime? 561 100)


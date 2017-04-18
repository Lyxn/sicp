#lang racket

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (* 8 (sum pi-term a pi-next b)))

;Exercise 1.29
(define (cube x) (* x x x))

(define (integral f a b n)
  (define h (/ (- b a) n))
  (define (add2 x) (+ x h h))
  (* (/ h 3)
     (+
      (* 4 (sum f (+ a h) add2 (- b h)))
      (* 2 (sum f (+ a h h) add2 (- b h)))
      (f a)
      (f b))
     ))

;Exercise 1.32
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
         (accumulate combiner null-value term (next a) next b))))

(define (acc-it combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))
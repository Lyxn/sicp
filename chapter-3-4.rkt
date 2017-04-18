#lang r5rs

;----stream----
(define nil '())
(define true #t)
(define false #f)

(define-syntax cons-stream
  (syntax-rules ()
    ((_ A B) (cons A (delay B)))))

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))

(define the-empty-stream '())
(define stream-null? null?)

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc . s)
  (if (stream-null? (car s))
      the-empty-stream
      (cons-stream (apply proc (map stream-car s))
                   (apply stream-map (cons proc (map stream-cdr s))))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

;(stream-car (stream-cdr (stream-filter odd? (stream-enumerate-interval 10000 100000))))

;----Exercise----
;Exercise 3.51
(define (show x)
  (display-line x)
  x)

;(define x (stream-map show (stream-enumerate-interval 0 10)))
;(stream-ref x 5)
;(stream-ref x 7)

(define (average x y) (/ (+ x y) 2.0))
(define (sqrt-improve guess x)
  (average guess (/ x guess)))
(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)

;(display-stream (sqrt-stream 2))
(define (divisible? a b)
  (= (remainder a b) 0))
(define (square x) (* x x))
(define (prime? n)
  (define (iter ps)
    (display-line (car ps))
    (cond ((> (square (stream-car ps)) n) true)
          ((divisible? n (stream-car ps)) false)
          (else (iter (stream-cdr ps)))))
  (iter primes))

(define primes
  (cons-stream
   2
   (stream-filter prime? (integers-starting-from 3))))

(define (add-streams a b)
  (stream-map + a b))
(define (mul-streams a b)
  (stream-map * a b))
(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

;Exercise 3.54
(define factorials (cons-stream 1 (mul-streams factorials integers)))

;Exercise 3.55
(define (partial-sums stream)
  (add-streams stream (cons-stream 0 (partial-sums stream))))

;Exxercise 3.56
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream s1car (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream s2car (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream s1car
                               (merge (stream-cdr s1)
                                      (stream-cdr s2)))))))))

(define s1 (cons-stream 1 (merge
                           (merge
                            (scale-stream s1 2)
                            (scale-stream s1 3))
                           (scale-stream s1 5))))

;Exercise 3.58 rational number 
(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

(define e1 (expand 3 7 10))
(define e2 (expand 3 8 10))

;Exercise 3.59
(define (integrate-series stream)
  (stream-map (lambda (a n) (/ a n)) stream integers))

(define (differential-series stream)
  (stream-map (lambda (a n) (* a n)) (stream-cdr stream) integers))

(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(define cosine-series
  (cons-stream 1 (integrate-series (stream-map - sine-series))))
(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

;Exercise 3.60
(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-streams (mul-series (stream-cdr s1) s2)
                            (scale-stream (stream-cdr s2) (stream-car s1)))))

(define os
  (add-streams (mul-series sine-series sine-series)
               (mul-series cosine-series cosine-series)))

;Exercise 3.61
(define (invert-unit-series stream)
  (cons-stream 1 (mul-series (stream-map - (stream-cdr stream))
                             (invert-unit-series stream))))

;Exercise 3.62
(define (div-series s1 s2)
  (mul-series s1 (invert-unit-series s2)))
(define tangent-series (div-series sine-series cosine-series))



(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))
(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))           ; Sn-1
        (s1 (stream-ref s 1))           ; Sn
        (s2 (stream-ref s 2)))          ; Sn+1  
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform
                             (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))

;Exercise 3.64
(define (stream-limit stream tolerance)
  (let ((s0 (stream-car stream))
        (s1 (stream-cdr stream)))
    (if (< (abs (- s0 (stream-car s1))) tolerance)
        (stream-car s1)
        (stream-limit s1 tolerance))))

;(define (sqrt x tolerance)
;  (stream-limit (sqrt-stream x) tolerance))

;Exercise 3.65
(define (ln2 n)
  (cons-stream (/ 1.0 n) (stream-map - (ln2 (+ n 1)))))
(define ln2-stream
  (partial-sums (ln2 1)))

;(display-stream (accelerated-sequence euler-transform ln2-stream))
(define (ln2v tolerance)
  (stream-limit (accelerated-sequence euler-transform ln2-stream) tolerance))


(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

(define (cartesian s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (interleave
     (stream-map (lambda (x) (list (stream-car s) x))
                 (stream-cdr t))
     (stream-map (lambda (x) (list x (stream-car t)))
                 (stream-cdr s)))
    (pairs (stream-cdr s) (stream-cdr t)))))

(define int-pairs (pairs integers integers))
(define all-pairs (cartesian integers integers))


(define (epairs s t)
  (interleave
   (stream-map (lambda (x) (list (stream-car s) x))
               t)
   (epairs (stream-cdr s) (stream-cdr t))))

(define (triples s t u)
  (let ((s0 (stream-car s))
        (s1 (stream-cdr s))
        (t0 (stream-car t))
        (t1 (stream-cdr t))
        (u0 (stream-car u))
        (u1 (stream-cdr u)))
    (cons-stream
     (list  s0 t0 u0)
     (interleave
      (triples s1 t1 u1)
      (interleave
       (stream-map (lambda (x) (list s0 t0 x)) u1)
       (stream-map (lambda (x) (cons s0 x)) (pairs t1 u1)))))))

(define int-triples (triples integers integers integers))

(define (pythagorean? tri)
  (let ((x (car tri))
        (y (cadr tri))
        (z (caddr tri)))
    (= (+ (square x) (square y)) (square z))))
(define pythagorean (stream-filter pythagorean? int-triples))


;Exersice 3.70
(define (merge-weight weight s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< (weight s1car) (weight s2car))
                  (cons-stream s1car
                               (merge-weight weight (stream-cdr s1) s2)))
                 (else
                  (cons-stream s2car
                               (merge-weight weight s1 (stream-cdr s2)))))))))

(define (weight-pairs weight s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weight weight
                 (stream-map (lambda (x) (list (stream-car s) x))
                             (stream-cdr t))
                 (weight-pairs weight (stream-cdr s) (stream-cdr t)))))

(define (sum x)
  (if (null? x)
      0
      (+ (car x) (sum (cdr x)))))
(define order-pairs (weight-pairs sum integers integers))

(define (check? x)
  (cond ((divisible? x 2) false)
        ((divisible? x 3) false)
        ((divisible? x 5) false)
        (else true)))
(define myints
  (stream-filter check? integers))

(define (myweight x)
  (let ((i (car x))
        (j (cadr x)))
    (+ (* 2 i) (* 3 j) (* 5 i j))))
(define mypairs (weight-pairs myweight myints myints))

;Exercise 3.71
(define (cube-sum x)
  (let ((i (car x))
        (j (cadr x)))
    (+ (* i i i) (* j j j))))

(define cube-pairs (weight-pairs cube-sum integers integers))
(define cube-num
  (stream-map cube-sum cube-pairs))

(define cube-count
  (cons-stream
   (cons (stream-car cube-num) 1)
   (stream-map (lambda (x y) (if (= (car x) y)
                                 (cons y (+ (cdr x) 1))
                                 (cons y 1)))
               cube-count
               (stream-cdr cube-num))))
(define ramanujan
  (stream-filter (lambda (x) (= (cdr x) 2)) cube-count))

;Exercise 3.72
(define (square-sum x)
  (let ((i (car x))
        (j (cadr x)))
    (+ (* i i) (* j j))))
(define square-pairs (weight-pairs square-sum integers integers))
(define square-triples
  (stream-map (lambda (x) (cons (square-sum x) x))
              square-pairs))
(define (sfun x y z)
  (let ((ax (car x))
        (ay (car y))
        (az (car z)))
    (if (and (= ax ay) (= ax az))
        (list x y z)
        nil)))
(define square-num
  (let ((sdt (stream-cdr square-triples)))
    (stream-filter
     (lambda (x) (not (null? x)))
     (stream-map sfun
                 square-triples
                 sdt
                 (stream-cdr sdt)))))

;Exercise 3.77
(define (integral delayed-integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (let ((integrand (force delayed-integrand)))
                   (add-streams (scale-stream integrand dt)
                                int))))
  int)
(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

;Exercise 3.81
(define random-init 1024)
(define (rand-update x)
  (let ((a 33797)
        (b 1)
        (m (expt 2 32)))
    (remainder (+ (* x a) b) m)))
(define rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))
(define random-numbers
  (cons-stream random-init
               (stream-map rand-update random-numbers)))

(define (map-successive-pairs f s)
  (cons-stream
   (f (stream-car s) (stream-car (stream-cdr s)))
   (map-successive-pairs f (stream-cdr (stream-cdr s)))))

(define cesaro-stream
  (map-successive-pairs (lambda (r1 r2) (= (gcd r1 r2) 1))
                        random-numbers))

(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
     (/ passed (+ passed failed))
     (monte-carlo
      (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

(define pi
  (stream-map (lambda (p) (sqrt (/ 6 p)))
              (monte-carlo cesaro-stream 0 0)))


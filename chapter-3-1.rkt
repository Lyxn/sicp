#lang racket
(define balance 100)

(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))

(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)

;----Exercise----
;Exercise 3.1
(define (make-accumulator sum)
  (lambda (y) (begin (set! sum (+ sum y))
                     sum)))

;Exercise 3.2
(define (make-monitored func)
  (define count 0)
  (define (reset) (set! count 0))
  (define (incre) (set! count (+ count 1)))
  (define (dispatch m)
    (cond ((eq? m 'reset-count) (reset))
          ((eq? m 'how-many-calls?) count)
          (else (begin (incre)
                       (func m)))))
  dispatch)

;(define s (make-monitored sqrt))
;(s 100) (s 100) (s 100) (s 100)
;(s 'how-many-calls?)
;(s 'reset-count)
;(s 'how-many-calls?)
;(define A (make-accumulator 5))
;(A 10)


;Exercise 3.3
(define (make-account-secret balance password)
  (define count 0)
  (define (reset) (set! count 0))
  (define (incre) (set! count (+ count 1)))
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (joint passwd)
    (disp passwd))
  (define (disp passwd)
    (lambda (pwd m)
      (if (equal? pwd passwd)
          (cond
            ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'joint) joint)
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m)))
          (begin (incre)               
                 (if (< count 2)
                     (error "Incorrect password")
                     (error "Call the cops")))))) 
  (disp password))


;(define acc (make-account-secret 100 '0123))
;((acc '0123 'withdraw) 40)
;((acc 'some-other-password 'deposit) 50)

;Exercise 3.5
(define (rand-update x)
  (let ([a 33797]
        [b 1]
        [m (expt 2 32)])
    (remainder (+ (* x a) b) m)))
(define rand
  (let ((x (current-seconds)))
    (lambda ()
      (set! x (rand-update x))
      x)))
(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))
(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))
(define square (lambda (x) (* x x)))
(define (area-test)
  (let ((x (random-in-range 2 8))
        (y (random-in-range 4 10)))
    (<= (+ (square (- x 5)) (square (- y 7))) 9)))
(define (area-pi trials)
  (* 36.0 (monte-carlo trials area-test)))
;(area-pi 10000000)

;Exercise 3.6
(define rad
  (let ([seed (current-seconds)])
    (define (dispatch m)
      (cond ((eq? m 'generate)
             (let ([x (rand-update seed)])
               (begin (set! seed x) x)))
            ((eq? m 'reset)
             (lambda (x) (set! seed x)))))
    dispatch))

;Exercise 3.7 false
(define (make-joint acc oldpwd newpwd)
  ((acc oldpwd 'joint) newpwd))

(define peter-acc (make-account-secret 100 '0123))
(define paul-acc
  (make-joint peter-acc '0123 'rosebud))

((peter-acc '0123 'withdraw) 5)
((paul-acc 'rosebud 'withdraw) 10)

;Exercise 3.8
(define f
  (let ([b 0])
    (lambda (x) (begin (set! b (- 1 b))
                       (* x b)))))

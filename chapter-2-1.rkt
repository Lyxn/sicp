#lang racket

;(define x (cons 1 2))
;(define y (cons 3 4))
;(define z (cons x y))

(define (make-rat n d)
  (define (neg? n d)
    (xor (> n 0) (> d 0)))
  (let ((g (gcd n d))
        (na (abs n))
        (da (abs d)))
    (if (neg? n d)
        (cons (- (/ na g)) (/ da g))
        (cons (/ na g) (/ da g)))))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

;Exercise 2.4
(define (mcons x y)
  (* (expt 2 x) (expt 3 y)))
(define (root x n)
  (if (= (remainder x n) 0)
      (+ 1 (root (/ x n) n))
      0))
(define (mcar z)
  (root z 2))
(define (mcdr z)
  (root z 3))

;Exercise 2.5
(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))
(define (double x) (* 2 x))
(define one (add-1 zero))
(define two (add-1 one))
(define fone (lambda (f) (lambda (x) (f x))))
(define ftwo (lambda (f) (lambda (x) (f (f x)))))
(define (fadd a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))
(define three (fadd one two))
(define four (fadd two two))

;Excercise 2.17
(define (last p)
  (if (null? (cdr p))
      (car p)
      (last (cdr p))))
;Excercise 2.18
(define (rev p)
  (define (iter p answer)
    (if (null? p)
        answer
        (iter (cdr p) (cons (car p) answer))))
  (iter p null))
(define (rrev p)
  (if (null? p)
      null
      (cons (car p) (rrev (cdr p)))))


;Excercise 2.19
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))
(define (no-more? p) (null? p))
(define (except-first-denomination p)
  (cdr p))
(define (first-denomination p)
  (car p))

;Exercise 2.20
(define (same-parity x . p)
  (define (iter p)
    (cond ((null? p) null)
          ((not (xor (even? x) (even? (car p))))
           (cons (car p) (iter (cdr p))))
          (else (iter (cdr p)))))
    (cons x (iter p)))


;Exercise 2.22
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things) 
              (cons (square (car things))
                    answer))))
  (iter items null))
(define (fsquare-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (cons (square (car things))
              (iter (cdr things) answer))))
  (iter items null))
(define (rsquare-list things)
  (if (null? things)
      null
      (cons (square (car things))
            (rsquare-list (cdr things)))))

;Exercise 2.23
(define (for-each f items)
  (cond ((null? items) true)
        (else (f (car items))
              (for-each f (cdr items)))))
;Exercise 2.27
(define (deep-reverse p)
  (define (iter p answer)
    (if (null? p)
        answer
        (iter (cdr p) (cons (rev (car p)) answer))))
  (iter p null))
;Exercise 2.28
(define (fringe tree)
  (cond ((null? tree)
         null)
        ((not (pair? tree)) (list tree))
        (else (append (fringe (car tree))
                      (fringe (cdr tree))))))

;Exercise 2.30
(define (square x) (* x x))
(define (recur-tree tree f)
  (cond ((null? tree) null)
        ((not (pair? tree)) (f tree))
        (else (cons (recur-tree (car tree) f)
                    (recur-tree (cdr tree) f)))))
(define (map-tree tree f)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (map-tree sub-tree f)
             (f sub-tree)))
       tree))
(define (square-tree tree)
  (map-tree tree square))
(define tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))

;Exercise 2.31
(define (subsets s)
  (if (null? s)
      (list null)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (l) (cons (car s) l)) rest)))))

; Sequence
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low (enumerate-interval (+ low 1) high))))

;Exercise 2.33
(define (amap p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) null sequence))
(define (fappend seq1 seq2)
  (accumulate cons seq2 seq1))
(define (flength sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

;Exercise 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))
;Exercise 2.35
(define (count-leaves t)
  (accumulate (lambda (x y) (+ x y))
              0
              (map
               (lambda (x) (if (pair? x)
                               (count-leaves x)
                               1))
               t)))
;Exercise 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      null
      (cons (accumulate op init (map (lambda (l) (car l)) seqs))
            (accumulate-n op init (map (lambda (l) (cdr l)) seqs)))))
(define s (list '(1 2 3) '(4 5 6) '(7 8 9) '(10 11 12)))
;Exercise 2.37
(define w1 (list '(1 2 3 4) '(4 5 6 6) '(6 7 8 9)))
(define v1 (list 1 2 3 4))
(define (dot-product v w)
  (accumulate + 0 (map * v w)))
(define (matrix-*-vector m v)
  (map (lambda (w) (dot-product w v)) m))
(define (transpose mat)
  (accumulate-n (lambda (x y) (cons x y)) null mat))
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (v) (matrix-*-vector cols v)) m)))


;Exercise 2.38
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))
(define (fold-right op initial sequence)
  (define (iter initial rest)
    (if (null? rest)
        initial
        (op (car rest) (iter initial (cdr rest)))))
  (iter initial sequence))

(define (reverse-right sequence)
  (fold-right (lambda (x y) (append y (list x))) null sequence))
(define (reverse-left sequence)
  (fold-left (lambda (x y) (cons y x)) null sequence))


;Nested Mapping
(define (flatmap proc seq)
  (accumulate append null (map proc seq)))

(define (permutations s)
  (if (null? s)                    ; empty set?
      (list null)                   ; sequence containing empty set
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))
(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

;Exercise 2.40
(define (proc i)
  (map (lambda (j) (list i j))
       (enumerate-interval 1 (- i 1))))
(define (unique-pairs n)
  (flatmap proc (enumerate-interval 1 n)))

;Exercise 2.41
(define (tproc k)
  (map (lambda (p) (cons k p))
       (unique-pairs (- k 1))))
(define (unique-triples n)
  (flatmap tproc (enumerate-interval 1 n)))
(define (target-sum-triples n s)
  (filter (lambda (p) (= s (accumulate + 0 p)))
          (unique-triples n)))

;Exercise 2.42
(define empty-board null)
(define (adjoin-position new-row k rest-of-queens)
  (cons new-row rest-of-queens))
(define (safe? k positions)
  (define (safe-diag? x0 y0 x1 y1)
    (cond ((and (= x0 y0)
                (= x1 y1))
           false)
          ((and (= 9 (+ x0 y0))
                (= 9 (+ x1 y1)))
           false)
          (else true)))
  (define (iter new olds)
    (cond ((null? olds) true)
          ((= new (car olds))
           false)
          ((not (safe-diag? new k (car olds) (length olds)))
           false)
          (else (iter new (cdr olds)))))
  (iter (car positions) (cdr positions)))
          
(define (queens board-size)
  (define (queen-cols k)  
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))
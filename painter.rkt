#lang racket

(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

;----TextBook----
(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller))
        ;(beside painter smaller)
        )))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller))
        ;(below painter smaller)
        )))
		
(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

;----Exercise----
;Exercise 2.45
(define (split x y)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split x y) painter (- n 1))))
          (x painter (y smaller smaller))))))
(define r-split (split beside below))
(define u-split (split below beside))

;Exercise 2.46
(define make-vect cons)
(define xcor-vect car)
(define ycor-vect cdr)
(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))
(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
             (- (ycor-vect v1) (ycor-vect v2))))
(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))
			 
;Exercise 2.47
(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))
(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
(define origin-frame car)
(define edge1-frame (lambda (f) (car (cdr f))))
(define edge2-frame (lambda (f) (cdr (cdr f))))

(define origin (make-vect 0.5 0.5))
(define edge1 (make-vect 0.5 0))
(define edge2 (make-vect 0 0.5))
(define a-frame (make-frame origin edge1 edge2))

;Exercise 2.49
(define (pframe f)
  (define s1 (make-segment (origin-frame f) (edge1-frame f)))
  (define s2 (make-segment (origin-frame f) (edge2-frame f)))
  (segments->painter (list s1 s2)))

(define (l2v l)
  (make-vect (car l) (second l)))
(define (line2seg points)
  (define (iter start vs) 
    (if (null? vs)
        null
        (cons (make-segment (l2v start)
                            (l2v (car vs)))
              (iter (car vs) (cdr vs)))))
  (iter (car points) (cdr points)))

(define x1 (list '(0 0) '(1 1)))
(define x2 (list '(0 1) '(1 0)))
(define px
  (let ((s1 (line2seg x1))
        (s2 (line2seg x2)))
    (segments->painter (append s1 s2))))

(define dm (list '(0 0.5) '(0.5 0) '(1 0.5) '(0.5 1) '(0 0.5)))
(define pdm
  (let ((s (line2seg dm)))
    (segments->painter s)))

(define w1 (list '(0 26) '(5 18) '(9 20) '(12 20) '(11 26) '(12 31)))
(define w2 (list '(0 20) '(5 13) '(9 18) '(11 16) '(9 0)))
(define w3 (list '(13 0) '(16 9) '(19 0)))
(define w4 (list '(24 0) '(20 15) '(31 6)))
(define w5 (list '(20 31) '(21 26) '(20 20) '(26 20) '(31 12)))
(define w6 (list '(14 24) '(16 22) '(18 24)))
(define w7 (list '(13 28) '(14 29) '(15 28)))
(define w8 (list '(17 28) '(18 29) '(19 28)))
(define (reg w)
  (map (lambda (l)
         (map (lambda (x)
                (/ x 31.0))
              l))
       w))
(define wave
  (let ((s1 (line2seg (reg w1)))
        (s2 (line2seg (reg w2)))
        (s3 (line2seg (reg w3)))
        (s4 (line2seg (reg w4)))
        (s5 (line2seg (reg w5)))
        (s6 (line2seg (reg w6)))
        (s7 (line2seg (reg w7)))
        (s8 (line2seg (reg w8))))            
    (segments->painter (append s1 s2 s3 s4 s5 s6 s7 s8))))

;Exercise 2.52
(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (sl painter n)
  (let ((combine4 (square-of-four flip-vert rotate180
                                  identity flip-horiz)))
    (combine4 (corner-split painter n))))

;----TestScript----
;(paint (pframe a-frame))
;(paint px)
;(paint pdm)
;(paint wave)
(paint (sl wave 4))
;(paint (right-split einstein 4))
;(paint (corner-split einstein 4))
(paint (sl einstein 4))
;(paint (r-split einstein 4))
;(paint (u-split einstein 4))
;(paint (flipped-pairs einstein))
#lang racket

; Huffman Tree
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

; Insertion Sort
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)    ; symbol
                               (cadr pair))  ; frequency
                    (make-leaf-set (cdr pairs))))))

;----Exercise----
;Exercise 2.67
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              ;(es (car message) tree)
              (encode (cdr message) tree))))

(define (element-of-set? symbol set)
  (cond ((null? set) false)
        ((eq? (car set) symbol) true)
        (else (element-of-set? symbol (cdr set)))))

(define (encode-symbol symbol tree)
  (define (iter symbol tree bits)
    (cond ((leaf? tree) bits)
          ((element-of-set? symbol (symbols (left-branch tree)))
           (iter symbol (left-branch tree) (cons 0 bits)))
          ((element-of-set? symbol (symbols (right-branch tree)))
           (iter symbol (right-branch tree) (cons 1 bits)))
          (else (error "Symbol miss" symbol))))
  (reverse (iter symbol tree '())))

(define (es symbol tree)
  (define (iter stack bits)
    (cond ((null? stack)
           (error "Symbol miss" symbol))
          (else
           (let ((tree (car stack))
                 (unstack (cdr stack))
                 (bit (car bits))
                 (unbits (cdr bits)))
             (cond ((not (leaf? tree))
                    (iter
                     (cons (left-branch tree) (cons (right-branch tree) unstack))
                     (cons (cons 0 bit) (cons (cons 1 bit) unbits))))
                   (else (if (equal? symbol (symbol-leaf tree))
                             bit
                             (iter unstack unbits))))))))
  (cdr (reverse (iter (list tree) '((0))))))

;Exercise 2.68
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge set)
  (if (> (length set) 1)
      (successive-merge
       (adjoin-set (make-code-tree (car set) (cadr set))
                   (cddr set)))
      (car set)))

(define words '((A 2) (NA 16) (BOOM 1) (SHA 3) (GET 2) (YIP 9) (JOB 2) (WAH 1)))
(define word-tree (generate-huffman-tree words))
(define lyrics '(GET A JOB 
SHA NA NA NA NA NA NA NA NA
GET A JOB 
SHA NA NA NA NA NA NA NA NA
WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP 
SHA BOOM))
(equal? (decode (encode lyrics word-tree) word-tree) lyrics)
;(make-leaf-set '((A 4) (B 2) (C 1) (D 1)))
;(generate-huffman-tree '((A 4) (B 2) (C 1) (D 1)))
;(encode-symbol 'B sample-tree)
;(decode sample-message sample-tree)
;(encode (decode sample-message sample-tree) sample-tree)

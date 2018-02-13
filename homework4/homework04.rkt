;;
;; FILE:     homework04.rkt
;; AUTHOR:   Taylor Olson
;; DATE:     2/12/18
;; COMMENT:  This module defines the five functions specified in
;;           Homework 4 as an importable module.
;;
;; MODIFIED: 2/12/18
;; CHANGE:   
;;

#lang racket
(require rackunit)      ; enables you to use rackunit tests
(provide string*        ; exports your functions to client code
         reject interleave every? positions-of)

;; --------------------------------------------------------------------------
;; Problem 1                                              (counted recursion)
;; --------------------------------------------------------------------------

(define string*
  (lambda (str n)
    (if (zero? n)
        ""
        (string-append str (string* str (sub1 n)))
      )))

(check-equal? (string* "Taylor" 3) "TaylorTaylorTaylor")
(check-equal? (string* "Jacob" 1) "Jacob")
(check-equal? (string* "lebron" 2) "lebronlebron")

;; --------------------------------------------------------------------------
;; Problem 2                                           (structural recursion)
;; --------------------------------------------------------------------------

(define reject
  (lambda (test? lst)
    (if (null? lst)
        '()
        (if (test? (first lst))
            (reject test? (rest lst))
            (append (list (first lst)) (reject test? (rest lst))))
   )))

(check-equal? (reject positive? '(1 2 -1 -2 3 -3 5 -4 4)) '(-1 -2 -3 -4))
(check-equal? (reject positive? '(-3 -4 -5 -6 -7 1 2 3)) '(-3 -4 -5 -6 -7))
(check-equal? (reject positive? '(1 2 3 4 5 6 7 8 9 10)) '())

;; --------------------------------------------------------------------------
;; Problem 3                                           (structural recursion)
;; --------------------------------------------------------------------------

(define interleave
  (lambda (lst1 lst2)
    ;; Handle lists of unequal lengths gracefully
    (if (<= (length lst1) (length lst2))
        (if (null? lst1)
            '()
            (append (list (cons (first lst1) (first lst2))) (interleave (rest lst1) (rest lst2)))
            )
        (if (null? lst2)
            '()
            (append (list (cons (first lst1) (first lst2))) (interleave (rest lst1) (rest lst2)))
            )
        )
    ))

(check-equal? (interleave '(a b c d e) '(1 2 3 4 5 6)) '((a . 1) (b . 2) (c . 3) (d . 4) (e . 5)))
(check-equal? (interleave '(a) '(1)) '((a . 1)))
(check-equal? (interleave '(a b c x) '(1 2 3 10)) '((a . 1) (b . 2) (c . 3) (x . 10)))

;; --------------------------------------------------------------------------
;; Problem 4                                           (structural recursion)
;; --------------------------------------------------------------------------

(define every?
  (lambda (lob)
    (if (null? lob)
        #t
        (if (first lob)
            (every? (rest lob))
            #f)
    )))

(check-equal? (every? (map (lambda (n) (<= n 40)) '(30 25 22 35 15))) #t)
(check-equal? (every? (map (lambda (n) (<= n 10)) '(5 25 15))) #f)
(check-equal? (every? (map (lambda (n) (>= n 32)) '(33 100 44))) #t)

;; --------------------------------------------------------------------------
;; Problem 5                   (structural recursion and interface procedure)
;; --------------------------------------------------------------------------
(define positions-of-helper
  (lambda (s los position)
    (if (null? los)
        '()
        (if (equal? s (first los))
            (append (list position) (positions-of-helper s (rest los) (add1 position)))
            (positions-of-helper s (rest los) (add1 position))
            )
        )))

(define positions-of
  (lambda (s los)
    (positions-of-helper s los 0)
    ))

(check-equal? (positions-of 'b '(a b a c b a g b t b)) '(1 4 7 9))
(check-equal? (positions-of 'x '(x y z)) '(0))
(check-equal? (positions-of 'w '(a b w c d w e f w)) '(2 5 8))
;; --------------------------------------------------------------------------


;;
;; FILE:     homework02.rkt
;; AUTHOR:   Taylor
;; DATE:     1/22/18
;; COMMENT: Provides templates for your solutions, plus a few tests.
;;
;; MODIFIED: 
;; CHANGE:   
;;

#lang racket
(require rackunit)      ; enables you to use rackunit tests

; -------------------
; -----   [1]   -----
; -------------------

(define inches->meters
  (lambda (inches)
    (/ inches 39.3701) ))                ; replace the 0 with your code

(define pounds->kilograms
  (lambda (pounds)
    (/ pounds 2.20462) ))                ; replace the 0 with your code

(check-equal? (inches->meters    39.3701) 1.0)
(check-equal? (inches->meters    78)      1.9811989301525776)
(check-equal? (pounds->kilograms 2.20462) 1.0)
(check-equal? (pounds->kilograms 237)     107.50151953624662)

; -------------------
; -----   [2]   -----
; -------------------

(define ladder-height
  (lambda (ladder-length base-distance)
    (sqrt (- (expt ladder-length 2) (expt base-distance 2)))))                ; replace the 0 with your code

(check-equal? (ladder-height 10 6)   8)
(check-equal? (ladder-height 13 5)   12)
(check-equal? (ladder-height 20 3.5) 19.691368667515217)

; -------------------
; -----   [3]   -----
; -------------------

(define candy-temperature
  (lambda (temp elevation)
    (- temp (/ elevation 500))))                ; replace the 0 with your code

; write your own check-equal? tests for the three examples

; let's pretend I didn't use your numbers to test because i'm lazy...
(check-equal? (candy-temperature 244 5280.0)   233.44)
(check-equal? (candy-temperature 302 977.69)   300.04462)
(check-equal? (candy-temperature 302 -1401.0) 304.802)


; -------------------
; -----   [4]   -----
; -------------------

(define in-range?
  (lambda (actual desired epsilon)
    (< (abs (- actual desired)) epsilon) ))               ; replace the #f with your code

(check-equal? (in-range? 4.95 5.0 0.1)  #t)
(check-equal? (in-range? 4.95 5.0 0.01) #f)     ;; not anymore!
(check-equal? (in-range? 5.0 4.95 0.1)  #t)     ;; works both ways
(check-equal? (in-range? 5.0 5.95 0.1)  #f)
(check-equal? (in-range? 5.5 5.95 0.5)  #t)

; -------------------
; -----   [5]   -----
; -------------------

(define body-mass-index
  (lambda (height weight)
    (/ (pounds->kilograms weight) (expt (inches->meters height) 2)) ))                ; replace the 0 with your code

; write your own check-equal? tests for the two examples given
; plus at least one more test of your own design
(check-equal? (body-mass-index 78 237.0)  27.3878810806232)
(check-equal? (body-mass-index 81 215.0)  23.03921698562725)
(check-equal? (body-mass-index 73 195)  25.72694298011893)

; -----   end   -----

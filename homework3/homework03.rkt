;;
;; FILE:     homework03.rkt
;; AUTHOR:   Taylor Olson
;; DATE:     2/11/18
;; COMMENT:  Includes rackunit tests for the examples in the assignment.
;;
;; MODIFIED: 
;; CHANGE:   
;;
#lang racket
(require rackunit)

; -----   [1]   -----

(define candy-temperature-at
  (lambda (elevation)
    'YOUR-CODE-HERE ))

  ; ----- tests -----

  (define temp-in-cf (candy-temperature-at 959))
  (check-equal? ((candy-temperature-at 5280) 302) 291.44)
  (check-equal? (temp-in-cf 240)                  238.082)

; -----   [2]   -----

(define in-range-of?
  (lambda (epsilon)
    'YOUR-CODE-HERE ))

  ; ----- tests -----

  (check-equal? ((in-range-of? 0.1) 4.95 5.0)  #t)
  (check-equal? ((in-range-of? 0.1) 5.0 4.85)  #f)

  (define within-0.01? (in-range-of? 0.01))
  (check-equal? (within-0.01? 4.95 5.0) #f)         ;; not anymore!
  (check-equal? (within-0.01? 5.0 4.99) #t)


; -----   [3]   -----

(define average-weight
  (lambda (height-weight-list)
    'YOUR-CODE-HERE ))

  ; ----- tests -----

  (check-equal? (average-weight '((79 . 225)))
                225.0)
  (check-equal? (average-weight '((70 . 150) (62 . 100)))
                125.0)
  (check-equal? (average-weight '((76 . 195) (81 . 212) (79 . 225) (78 . 206)))
                209.5)

; -----   [4]   -----

(define total-error
  (lambda (list-of-games)
    'YOUR-CODE-HERE ))

  ; ----- tests -----

  (check-equal? (total-error '((2 -7) (-4 -20) (7 8) (-13 2)))
               41)

; -----   [5]   -----

(define max-open-seats
  (lambda (sections)
    'YOUR-CODE-HERE ))

  ; ----- tests -----

  (define example
    '(("Dept" "Number" "Section" "Class Nbr" "Capacity" "Enrollment")
      ("CS" "1000" "1" "11546" "30" "30")
      ("CS" "1025" "1" "11547" "30" "30")
      ("CS" "1120" "1" "11557" "30" "15")
      ("CS" "1130" "1" "11548" "30" "18")))
  (check-equal? (max-open-seats example) 15)

; -----   end   -----


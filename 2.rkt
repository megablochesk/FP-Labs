; Volodymyr Shevchenko IPZ-41 Lab 1 Variant 1
#lang racket

(define (displayAll . vs)
  (for-each display vs))

; Task 1 functions
(define prec (expt 10 -6))

(define (fact x)
   (if (= x 0)
       1
       (* x (fact (- x 1)))
   )
 )

(define (customCos x precision) (customCosHelper x 0 precision))

(define (customCosHelper x num precision)
   (let ((current (/ (expt (- (* x x)) num) (fact (* 2 num)))))
      (if (< (abs current) precision)
          current
          (+ current (customCosHelper x (+ num 2) precision))
          )
     )
  )

(define (f x)
          (if (equal? (customCos (* x x) prec) 0)
              "Nan"
  (cond
    ((and (<= -1 x) (<= x 0))
     (/ (customCos (* x 0.5) prec) (customCos (* x x) prec))
     )
    ((and (< 0 x))
     (* (customCos (* x 0.5) prec) (customCos (* x 0.5) prec) (customCos(* x 2) prec))
     )
   )))
 
(define (fControl x)
            (if (= (customCos (* x x) prec) 0)
              "Nan"
  (cond
    ((and (<= -1 x) (<= x 0))
     (/ (cos (* x 0.5)) (cos (* x x)))
     )
    ((and (< 0 x))
     (* (cos (* x 0.5)) (cos (* x 0.5)) (cos (* 2 x)))
     ))
   )
 )
(define (task1Print a b step)
  (begin
    (if (<= a b)
    (begin
      (displayAll "Result for " a ": " (f a) ". Error: " (- (f a) (fControl a)) "\n")
      (task1Print (+ a step) b step)
     )
    (printf "End")))
)

; Task 2 functions

(define (chainFraction k c)
  (if (= k 3)
       (+ 1 (/ 1 c))
       (chainFraction (- k 2) (+ (- k 2) (/ 1 c)))
  )
)
; Main function sequence
(display "\nTask 1\n\n")
(printf "\nEnter border a: ")
(define a (read))
(printf "\nEnter border b: ")
(define b (read))
(printf "\nEnter step: ")
(define step (read))

(task1Print a b step)

(display "\nTask 2 \n\n")

(display "Please enter n: ")
(define n (read))

(displayAll "Chain fraction: " (chainFraction (+ 1 n n) (+ 1 n n)) "\n")
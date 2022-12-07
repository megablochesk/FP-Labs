#lang racket

;Shevchenko Volodymyr IPZ-41 Lab 5.1 variant 1

(define (displayAll . vs)
  (for-each display vs))

(displayAll "\nIPZ-41 Volodymyr Shevchenko Lab 5.1 variant 1")
(displayAll "\nYou will get answer for a/b,c/d as gcd(a, c)/lcm(b, d)")
(printf "\nEnter a: ")
(define a (read))
(printf "\nEnter b: ")
(define b (read))
(printf "\nEnter c: ")
(define c (read))
(printf "\nEnter d: ")
(define d (read))

(define nokc 0)
(define nokz 0)

(define (main)
  (displayAll "\nAnswer: " (calculate-lcm a b c d))
  )

(define (calculate-lcm a b c d) ;To find lcm for rational numbers
  (begin
    (set! nokz (lcm b d))
    (set! nokc (lcm (* a (/ (lcm b d) b)) (* c (/ (lcm b d) d))))
    (/ nokc nokz)
    )
  )
  

(main)
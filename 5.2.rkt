;1.1. 
;1.2. Створити список з комплексних чисел, заданих в алгебраїчній формі. Створити новий список, елементи якого є комплексні числа, переведені з алгебраїчної форми у тригонометричну. 
; Volodymyr Shevchenko IPZ-41 Lab 5 Variant 1

#lang racket

(define (calcSumOfFourFractions a b c d) (/ (+ (* a b c) (* d b c) (* a d c) (* a b d)) (* a b c d)))

(define (isOne number)
  (cond ((equal? number 1) (display "YES"))
        (else (display "NO"))))

(printf "\nEnter number a: ")
(define a (read))
(printf "\nEnter number b: ")
(define b (read))
(printf "\nEnter number c: ")
(define c (read))
(printf "\nEnter number d: ")
(define d (read))

; (calcSumOfFourFractions a b c d)
(isOne (calcSumOfFourFractions a b c d))
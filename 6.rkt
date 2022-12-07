;1.1. Створити вектор. Визначити максимальне та мінімальне значення серед елементів із парними та непарними індексами. Вивести мінімальний, максимальний елементи.
;1.2. Побудувати стек натуральних чисел. Вивести на екран створений стек. Надрукувати в зворотному порядку числа стеку, пропускаючи кратні заданому користувачем числу.
; Volodymyr Shevchenko IPZ-41 Lab 6 Variant 1

#lang racket

(define (displayAll . vs)
  (for-each display vs))

; Task 1 functions
(define (getFirst v) (vector-ref v 0))

(define (getTale v) (vector-take-right v (- (vector-length v) 1)))

(define (getVectorElements v isEvenElement)
  (cond ((and (> (vector-length v) 0) (= isEvenElement 1)) (list* (getFirst v) (getVectorElements (getTale v) 0)))
        ((and (> (vector-length v) 0) (= isEvenElement 0))                     (getVectorElements (getTale v) 1))
        (else (list))))


(define (minElement v) (inexact->exact (for/fold ([m +inf.0]) ([x (in-vector v)]) (min m x))))
(define (maxElement v) (inexact->exact (for/fold ([m -inf.0]) ([x (in-vector v)]) (max m x))))

; Task 2 functions

(define (getStack s) (reverse s))

(define (getStackWithCondition s a)
  (cond ((and (> (vector-length s) 0) (> (modulo (getFirst s) a) 0)) (list* (getFirst s) (getStackWithCondition (getTale s) a)))
        ((and (> (vector-length s) 0) (= (modulo (getFirst s) a) 0)) (                    getStackWithCondition (getTale s) a))
        (else (list))))


; Task 1
(define vf (vector 1 2 21 3 -10 4 5))

(define vfOdd (list->vector (getVectorElements vf 1)))
(define vfEven (list->vector (getVectorElements vf 0)))

(displayAll "Min value for odd elements: " (minElement vfOdd) "\n")
(displayAll "Max value for odd elements: " (maxElement vfOdd) "\n")

(displayAll "Min value for even elements: " (minElement vfEven) "\n")
(displayAll "Max value for even elements: " (maxElement vfEven) "\n")

; Task 2

(define stack (list 10 22 13 4 15 26 77 9 14))
(displayAll "Created stack: " (getStack stack) "\n")

(printf "\nEnter number: ")
(define a (read))

(displayAll "Reversed stack witout x/" a ": " (getStackWithCondition (list->vector stack) a))
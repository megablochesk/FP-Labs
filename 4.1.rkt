#lang racket

(define var 0)
(define main-list '())
(define drv-variable 0)
(define even-amount 0)
(define avrg 0)

(printf "\nVolodymyr Shevchenko IPZ-41 Lab 4.1 Variant 1")

;To decide what to do
(define (program-menu)
  (begin
(printf "\n 1 - Set element in the list
 2 - Insert value into list on position
 3 - Change all even values to the mean of all elements in the list
 4 - Reset list")
(printf "\n Your variant: ")
(set! var (read))
(start-variant var)
        )
)

;Branches with function calls
(define (start-variant v)
  (begin
    (cond
      
   ;Here we can send functions as parameters to call it later
      ;(Because we have the same structures of functions but different functionality)
   [(= v 1) (add-element-into-list set-at)]
   [(= v 2) (add-element-into-list insert-into-list)]
   ;--------------------------------------------------------------------------
   
   [(= v 3) (change-all-even-values 0)]
   [(= v 4) (reset-list-all-values-hng)])
    (display-list-parameters)
   (program-menu))
)

;Inserting value into list at defined position
(define (insert-into-list at val)
  (begin
    (define lst1 (take-right main-list (- (length main-list) at)))
    (define lst2 (drop-right main-list (- (length main-list) at)))
    (set! main-list (append lst2 (list val) lst1)))
         )

;To change all even values to average value
(define (change-all-even-values curr)
  (begin
    (if (< curr (length main-list))
  (if (is-even (list-ref main-list curr))
      (begin
        (set-at curr avrg)
        (change-all-even-values (+ curr 1)))
      (change-all-even-values (+ curr 1)))
  even-amount)
         )
  )

;To find average value
(define (average l)
  (if (= (length main-list) 0)
      0
      (/ (foldr (lambda (x y) (+ x y)) 0 l) 
     (length l))))

;To reset all values in list
(define (reset-list-all-values-hng)
  (set! main-list '()))

;To add element into value (or change old value on new)
(define (add-element-into-list func)
  (begin
    (printf "\nEnter the element: ")
    (define val (read))
    (printf "\nEnter the position: ")
    (define pos (read))
    (func pos val)
    ))

;To display all current parameters about list
(define (display-list-parameters)
  (begin
        (printf "\nYour list: ")
    (display main-list)
    (printf "\nAmount of values: ")
    (display (length main-list))
    (printf "\nAmount of even values: ")
    (display (count-all-even-values 0)))
    (printf "\nAverage value: ")
    (display (average main-list))
    (set! even-amount 0)
    (set! avrg (average main-list))
  )

;To set value on defined position in list
(define (set-at at val)
  (begin
  (if (< (length main-list) (+ at 1))
      (begin
        (set! main-list (append main-list (list 0)))
        (set-at at val)
                     )
      (set! main-list (list-set main-list at val))))
)


;To count all even values inside list
(define (count-all-even-values curr)
  (if (< curr (length main-list))
  (if (is-even (list-ref main-list curr))
      (begin
        (set! even-amount (+ even-amount 1))
        (count-all-even-values (+ curr 1)))
      (count-all-even-values (+ curr 1)))
  even-amount)
)

;Is current value even or not (Including decimal)
(define (is-even x)
  (if (integer? x)
  (if (equal? (remainder x 2) 0)
      #t
      #f)
  #f))

(program-menu)
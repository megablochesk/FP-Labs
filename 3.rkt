#lang racket

; Volodymyr Shevchenko IPZ-41 Lab 3

;Task 1.1
(define a -1.0)
(define b 1.0)
(define step (expt 10.0 -6))
(define eps (expt 10.0 -6))

(define (check-error current)
  (if (<= ((lambda (answer eps) (- answer eps)) 0 eps) current)
      (if (<= current ((lambda (answer eps) (+ answer eps)) 0 eps))
          #t
          #f)
      #f)
  )

(define (get-answer-using-enumeration a b step current)
    (if (< b (+ current step))
        "Not found"
        (find-answer-enumeration current step))
  )

(define (find-answer-enumeration current step)
  (if (check-error (- (cos current 12) current))
      current
      (get-answer-using-enumeration a b step (+ current step)))
)

(define (cos x n)
  (define (hulp ctr res prevPow prevFac switch)
    (let ((switchOp (if (eq? (modulo switch 2) 0) + -)))
      (if (> ctr (+ 2 n))
          res
          (let ((newPow (* prevPow x x))
                (newFac (* (- ctr 1) ctr prevFac)))
            (hulp (+ ctr 2) (switchOp res (/ newPow newFac)) newPow newFac (+ switch 1))))))
  (hulp 2 1 1 1 1))

;Task1.2

(define x0 -10.0)
(define x1 10.0)
(define (f x)
  (define s (- (cos x 20) x))
  s)
(define xn 1.0)
(set! a -10.0)

(define condition #t)

(define (get-answer-using-chord x0 x1 step e)
  (if (equal? condition #t)
      (helper x0 x1 step e)
      xn)
  )

(define (helper x0 x1 step e)
   (set! xn (setx x0 x1))
   (set! x0 x1)
   (set! x1 xn)
   (set! a (+ a step))
   (set! condition (> (abs(f xn)) (abs e)))
   (get-answer-using-chord x0 x1 a e)
   )

(define (setx x0 x1)
  (- x0 (/ (* (- x1 x0) (f x0)) (- (f x1) (f x0)))))

;Task2

(define integral-accuracy 86.0) ;Const
(define h 0.0)
(define sum 0.0)
(set! a 0.0) ;Const
(set! b 3.1415) ;Const


;Incorrect
(define (find-integral a b n)
  (begin
   (set-h a b n)
  (sum-helper 0)))

(define xt 0.0)

(define (sum-helper k)
  (begin
  (set! xt (+ a (* k h)))
  (if (<= k integral-accuracy)
  (begin (if (= k 0)
      (set-sum (fs xt))
      (if (= k integral-accuracy)
          (set-sum (fs xt))
          (if (is-even k)
              (set-sum (* 4 (fs xt)))
              (set-sum (* 2 (fs xt)))))
  )
         ;(printf "f")
         (sum-helper (+ k 1)))
(* (/ h 3) sum))))

(define (set-sum x)
  (set! sum (+ sum x)))

(define (is-even i)
  (if (equal? (remainder i 2) 0)
      #t
      #f)
  )

(define (set-h a b n)
  (set! h (/ (- b a) n)))

(define (fs x)
  (if (> 1 x) (/ (cos x 60) (sqrt (- 1 (* x x))))
  0))

(printf "\nTask1")
(printf "\n Enumeration method answer (cos(x) = x): ")
(display (get-answer-using-enumeration a b step a))
(printf "\n Chord method answer (cos(x) = x): ")
(display (get-answer-using-chord x0 x1 step eps))
(printf "\nTask2")
(printf "\n integral(cos(x)/sqrt(1-x*x))dx: ")
(display (find-integral a b integral-accuracy))
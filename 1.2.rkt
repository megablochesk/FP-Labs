(define (degree x y) 
  (if (= y 0)
      1
      (* x (degree x (- y 1)))))

(define sum 1000)
(define pr 1.25)
(define t 12)

(define k (+ 1 (/ pr 100)))

(define koef (degree k t))

(* sum koef)
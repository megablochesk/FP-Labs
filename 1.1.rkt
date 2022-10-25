(define (f x) 
  (if (= x 0)
      0
      (+ (modulo x 10) (f (/ (- x (modulo x 10)) 10)))))

(f 1234)
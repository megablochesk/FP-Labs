#lang racket

(define (displayAll . vs)
  (for-each display vs))

(define allCarriages (list 1 1 1 2 2 1 2 1 2 2 1 2 1 1))
(define trainOne (list))
(define trainTwo (list))

(define timeToAddCarriageTypeOne 10)
(define timeToAddCarriageTypeTwo 15)

(define intervalBetweenTrains 0)

(define timeToReviewTrainOne 15)
(define timeToReviewTrainTwo 10)

(define summaryTimeForTrainOne 0)
(define summaryTimeForTrainTwo 0)

(define (start-program)
  (displayAll "Volodymyr Shevchenko IPZ-41 Lab 4.2")
  (sort-carriage 0)
  )

(define (sort-carriage carriageN)
  (begin
    (if (= carriageN (length allCarriages))
        (begin
          (displayAll "\nTrain 1: " trainOne "\nTrain 2: " trainTwo "\n\n")
          (set! summaryTimeForTrainOne (+ summaryTimeForTrainOne timeToReviewTrainOne))
          (set! summaryTimeForTrainTwo (+ summaryTimeForTrainTwo timeToReviewTrainTwo))
          (displayAll "\nTime before 1-st train will start: " summaryTimeForTrainOne)
          (displayAll "\nTime before 2-nd train will start: " summaryTimeForTrainTwo)
          (displayAll "\nTime difference between two trains: "
                      (abs (- summaryTimeForTrainOne summaryTimeForTrainTwo )))
         )
        (begin
          (check-type-of-carriage (list-ref allCarriages carriageN))
          (displayAll "\nTrain 1: " trainOne
                      "\nTrain 2: " trainTwo "\n\n")
          (sort-carriage (+ carriageN 1))
         )
        )
    ))

(define (check-type-of-carriage c)
  (if (= c 1)
      (begin
       (set! trainOne (append trainOne (list 1)))
       (set! summaryTimeForTrainOne
             (+ summaryTimeForTrainOne timeToAddCarriageTypeOne))
       (displayAll "Added carriage with type 1 to train 1")
       )
      (begin
        (set! trainTwo (append trainTwo (list 2)))
        (set! summaryTimeForTrainTwo
             (+ summaryTimeForTrainTwo timeToAddCarriageTypeTwo))
               (displayAll "Added carriage with type 2 to train 2")
        )))









(start-program)
#lang racket

  (require 2htdp/batch-io)

(define text "«Кто управляет прошлым, – гласит партийный лозунг, – тот управляет будущим; кто управляет настоящим, тот управляет прошлым».")
(define var "")
(define alphabet '("а" "б" "в" "г" "д" "е" "ё" "ж" "з" "и" "й" "к" "л" "м" "н" "о" "п" "р" "с" "т" "у" "ф" "х" "ц" "ч" "ш" "щ" "ъ" "ы" "ь" "э" "ю" "я"))
(define output "")
(define n 2)

;Lab 7 Aldokhin Volodymyr IPZ-41 variant 1
(define (displayAll . vs)
  (for-each display vs))

  (displayAll "\nText: \n" text)

(define (main)
  (displayAll "\n\n\nVolodymyr Shevchenko IPZ-41 Lab 7")

  (displayAll
   "\n1 - write to file
2 - read from file and encrypt
Enter the variant: ")
  (set! var (read))

  (if (= var 1)
      (begin
      (writeToFile)
      (readFromFile)
      (displayAll "Input text: " text)
      )
      (if (= var 2)
          (begin
            (set! output "")
            (printf "Enter the step 0 to 33:")
            (set! n (read))
            (caesar-encryption 0)
            (writeToFile output)
            )
          (main)
          )
      )
  
  (main)
  )

(define (caesar-encryption idx)
  (begin
    (if (< idx (string-length text))
        (begin
          (set! output (string-append output (get-necessary-letter (string-ref text idx) 0)))
              (caesar-encryption (+ idx 1)))
        (displayAll "\nOriginal: " text "\nEncrypted: " output)
        )
    )
  )

(define (get-necessary-letter let idx)
  (if (< idx (length alphabet))
  (if (equal? (list-ref alphabet idx) (string let))
        (list-ref alphabet (remainder (+ idx n) (length alphabet)))      
      (get-necessary-letter let (+ idx 1)))
      (string let))
      
  )

(define (readFromFile)
  (set! text "")
  (set! text (string-join (read-lines "text.txt") "\n"))
  )

(define (writeToFile text1)
(write-file "Output.txt" text1)
  )

(main)
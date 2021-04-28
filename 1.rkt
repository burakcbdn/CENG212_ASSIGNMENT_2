#lang racket


; Returns the square of the given number
(define (square x)
  * x x)

; Sorts the given list in ascending order
(define (getSorted list)
  (sort list <))

; Calculates sum of elements for variance
(define (sumForVariance list)
  (if (empty? list)
      0
      (+ (square (- (car list) (mean list))) (sumOfElements (cdr list)))))

; Sums all elements of elements in the list.
(define (sumOfElements list)
  (if (empty? list)
      0
      (+ (car list) (sumOfElements (cdr list)))))



; Reads the file
(define (readFile file)
  (file->lines file))

; Parses readed file as a 2 dimensional array
(define (changeToTwoDimension lines)
  (map (lambda (line) (split line ",")) lines))

; Converts 2 dimensional string array to 2 dimensional number array
(define (convertToNumArray array)
  (map (lambda (list) (map (lambda (value) (toNum value)) list)) array))

; splits the line from specified character
(define (split str splitter)
   (regexp-split splitter str))

; Converts string value to number
(define (toNum str)
  (string->number str))

; Parses readed file into useful data
(define (parseFile file)
  (define lines (readFile file))
  (define twoDimensionArray (changeToTwoDimension lines))
  (define numArray (convertToNumArray twoDimensionArray))
  numArray)

; Data to be used in operations
(define data (parseFile "wine.data.txt"))



; Gets all column of data from 2 dimensional data
(define (getElementsAtIndex array index)
  (define elementsAtIndex (map (lambda (list) (list-ref list index)) array))
  elementsAtIndex
  )



; Calculates the mean
(define (mean list)
  (define total (sumOfElements list))
  (define size (length list))
  (/ total size))


; Calculates the mod
(define (median list)
  (define sortedList (getSorted list))
  (define size (length list))
  (cond
    [(even? size) (/(+ (list-ref sortedList (/ size 2)) (list-ref sortedList (+ 1 (/ size 2)))) 2)]
     [else (list-ref sortedList (round (/ size 2)))]
      ))

; Calculates the variance
(define (variance list)
  (define total (sumForVariance list))
  (define size (length list))
  (/ total size))



; There is 14 column in each row but we do not need first one so
; I will use indexes 1-13
(define indexesToCalculate (list 1 2 3 4 5 6 7 8 9 10 11 12 13))

(display "Calculating Mean Median and Variance Values...")
(display "\n")
(display "\n")

;Displays the output as Mean, Median, Variance
(define (displayOutput data)
  (for-each (lambda (index) 
                        (display "Values for index ")
                        (display index)
                        (display "\n")
                        
                        (display "Mean: ")
                        (display (mean (getElementsAtIndex data index)))
                        (display " Median: ")
                        (display (median (getElementsAtIndex data index)))
                        (display " Variance: ")
                        (display (variance (getElementsAtIndex data index)))
                        (display "\n")
                        

                        ) indexesToCalculate))


(display "Summary Statistics: ")
(display "\n")

; Displaying first output (summary statistics)
(displayOutput data)


(display "\n")
(display "\n")


; Shuffles the list
(define shuffle
  (lambda (list)
    (if (< (length list) 2) 
        list
        (let ((item (list-ref list (random (length list)))))
          (cons item (shuffle (remove item list)))))))

(display "Training and testing data...")
(display "\n")

(define shuffledData (shuffle data))


; Spliting data into train and test data
(define (getTrainData data)
  (define trainData (take data (round (*(/ (length data) 5) 4))))
 trainData
  )

(define (getTestData data)
  (define testData (take-right data (round (*(/ (length data) 5) 1))))
  testData)

(define trainData (getTrainData shuffledData))
(define testData (getTestData shuffledData))


; Checks the random gues with the original class
(define (checkGuess data guess)

  (if (equal? (list-ref data 0) guess)
      1
      0))

; Calculates the accuracy of our guesses
(define (calculateAccuracy testData)
  ( if (empty? testData) 
       0
       (+ (checkGuess (car testData) (+ (random 3) 1)) (calculateAccuracy (cdr testData)))))

; Converting accuracy values into percentage
(define result (*(/ (calculateAccuracy testData) (length testData)) 100.0))

; Displaying final output
(display "Total accuracy of this operation is: ")
(display result)
(display "%")







  


  


  









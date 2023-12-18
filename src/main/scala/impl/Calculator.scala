package pp202302.project.impl

// Problem 4: Fill it!
val calculator: String = 
    s"""
    |(let
    |   (val numericStringList (cons "0" (cons "1" (cons "2" (cons "3" (cons "4" (cons "5" (cons "6" (cons "7" (cons "8" (cons "9" nil)))))))))))
    |   (val numericStringMapList (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 (cons 6 (cons 7 (cons 8 (cons 9 (cons 10 nil)))))))))))
    |   (val operatorList (cons "+" (cons "-" (cons "*" (cons "/" nil)))))
    |   (def customOr ( first second )
    |       (if first first second)
    |   )
    |   (def getValue ( target ref value )
    |       (if (= target ref)
    |           value
    |           0
    |       )
    |   )
    |   (def matchAndGetValue ( tester stringList stringMapList )
    |       (let 
    |           (val thisValue (app getValue (fst stringList) tester (fst stringMapList)))
    |           (if ( nil? (snd stringList) )
    |               thisValue
    |               (app customOr thisValue (app matchAndGetValue tester (snd stringList) (snd stringMapList)))
    |           )
    |       )
    |   )
    |   (def matchOperator ( tester stringList )
    |       (let
    |           (val thisValue (if (app getValue tester (fst stringList) 1)
    |               (fst stringList)
    |               nil
    |           ))
    |           (if (nil? thisValue)
    |               (if (nil? (snd stringList))
    |                   nil
    |                   (app matchOperator tester (snd stringList))
    |               )
    |               thisValue
    |           )
    |       )
    |   )
    |   (def getNumericValue ( target )
    |       ( if ( = 0 (app matchAndGetValue target numericStringList numericStringMapList))
    |           nil
    |           (- (app matchAndGetValue target numericStringList numericStringMapList) 1)
    |       )
    |   )
    |   (def checkIsOperator ( target )
    |       (let 
    |           (val operator (app matchOperator target operatorList))
    |           ( if ( nil? operator)
    |               nil
    |               operator
    |           )
    |       )
    |   )
    |   (def stringToInt ( preValue numericString )
    |       (if ( < (len numericString) 1)
    |           (cons preValue numericString)
    |           (let 
    |               (val thisChar (substr numericString 0 1))
    |               (val thisVal (app getNumericValue thisChar))
    |               (val nextString (substr numericString 1 (len numericString)))
    |               (if (nil? thisVal)
    |                   (cons preValue numericString)
    |                   (app stringToInt (+ (* preValue 10) thisVal) nextString)
    |               )
    |           )
    |       )
    |   )
    |   (def getIntAndContinue ( line sign f )
    |       (let
    |           (val processedValue (app stringToInt 0 line))
    |           (val leftString (snd processedValue))
    |           (val fetchedValue (* sign (fst processedValue)))
    |           (val isFailed (= (len line) (len leftString)))
    |           (val nextOperator (if (> (len leftString) 0)
    |               (substr leftString 0 1)
    |               nil
    |           ))
    |           (val nextLine (if (> (len leftString) 0)
    |               (substr leftString 1 (len leftString))
    |               ""
    |           ))
    |           (val result (if ( * (if isFailed 0 1) ( > (len nextLine) 0))
    |               (cons (cons fetchedValue nextOperator) (app f nextLine))
    |               (cons (cons fetchedValue nextOperator) nil)
    |           ))
    |           (if ( app customOr isFailed ( = (snd result) "parse error"))
    |               "parse error"
    |               result
    |           )
    |       )
    |   )
    |   (def splitNumericAndOperator ( line )
    |       (let
    |           (val firstChar (substr line 0 1))
    |           (val isOperator (app checkIsOperator firstChar))
    |           (val isNumericValue (app getNumericValue firstChar))
    |           (val isMinus (= isOperator "-"))
    |           (val sign (* -1 (- (* isMinus 2) 1)))
    |           (val isAvailableStart (app customOr (* (if (nil? isOperator) 0 1) isMinus) isNumericValue))
    |           (val leftString (if ( > (len line) 1)
    |               (substr line 1 (len line))
    |               ""
    |           ))
    |           (val result (if isAvailableStart
    |               (if (nil? isOperator)
    |                   (app getIntAndContinue line sign splitNumericAndOperator)
    |                   (app getIntAndContinue leftString sign splitNumericAndOperator)
    |               )
    |               "parse error"
    |           ))
    |           result
    |       )
    |   )
    |   (def crossProcess ( expression )
    |       nil
    |   )
    |   (def fullProcess ( expression )
    |       nil
    |   )
    |   (val exitString "exit")
    |   (val invokeCheck "sibal\n")
    |   (defIO runCycle ()
    |       (readline inputString)
    |       (print inputString)
    |       (print "\n")
    |       (print numericStringMapList)
    |       (print "\n")
    |       (print (substr "sibal" 0 1))
    |       (print "\n")
    |       (print (app stringToInt 0 "1234"))
    |       (print "\n")
    |       (print (app splitNumericAndOperator inputString))
    |       (print "\n")
    |       (if (= inputString exitString)
    |           nil
    |           (app runCycle)
    |       )
    |   )
    |   (defIO printRunCycleResult ()
    |       (print (app runCycle))
    |       nil
    |   )
    |  (app printRunCycleResult)
    |)
    |""".stripMargin

    /*
        stringToInt 최적화할 것.
    
    */
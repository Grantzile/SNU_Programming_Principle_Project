package pp202302.project.impl

// Problem 4: Fill it!
val calculator: String = 
    s"""
    |(let
    |   (val numericStringList (cons "0" (cons "1" (cons "2" (cons "3" (cons "4" (cons "5" (cons "6" (cons "7" (cons "8" (cons "9" nil)))))))))))
    |   (val numericStringMapList (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 (cons 6 (cons 7 (cons 8 (cons 9 (cons 10 nil)))))))))))
    |   (val operatorList (cons "+" (cons "-" (cons "*" (cons "/" nil)))))
    |   (val crossOperatorList (cons "*" (cons "/" nil)))
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
    |   (def checkIsCrossOperator ( target )
    |       (let 
    |           (val operator (app matchOperator target crossOperatorList))
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
    |               (if (nil? (snd result))
    |                   (if (nil? (snd (fst result)))
    |                       result
    |                       "parse error"
    |                   )
    |                   result
    |               )
    |           )
    |       )
    |   )
    |   (def splitNumericAndOperator ( line )
    |       (let
    |           (val firstChar (substr line 0 1))
    |           (val isNumericValue (app getNumericValue firstChar))
    |           (val sign 1)
    |           (val isAvailableStart (+ 1 isNumericValue))
    |           (val result (if isAvailableStart
    |               (app getIntAndContinue line sign splitNumericAndOperator)
    |               "parse error"
    |           ))
    |           result
    |       )
    |   )
    |   (def calculate ( firstOperand operator secondOperand)
    |       (if (= operator "+")
    |           (+ firstOperand secondOperand)
    |           (if (= operator "-")
    |               (- firstOperand secondOperand)
    |               (if (= operator "*")
    |                   (* firstOperand secondOperand)
    |                   (/ firstOperand secondOperand)
    |               )
    |           )
    |       )
    |   )
    |   (def crossProcess ( expression prevValue prevOperator )
    |       (let
    |           (val thisOperand (fst (fst expression)))
    |           (val operator (snd (fst expression)))
    |           (val matchedOperator (app checkIsCrossOperator operator))
    |           (val contExpression (snd expression))
    |           (val preprocessResult (app calculate prevValue prevOperator thisOperand))
    |           (val result (if (nil? contExpression)
    |               (cons (cons preprocessResult nil) nil)
    |               (if (nil? matchedOperator)
    |                   (cons (cons preprocessResult operator) (app crossProcess contExpression 1 "*"))
    |                   (app crossProcess contExpression preprocessResult operator)
    |               )
    |           ))
    |           result
    |       )
    |   )
    |   (def addSubProcess ( expression prevValue prevOperator )
    |       (let
    |           (val thisOperand (fst (fst expression)))
    |           (val operator (snd (fst expression)))
    |           (val contExpression (snd expression))
    |           (val preprocessResult (app calculate prevValue prevOperator thisOperand))
    |           (val result (if (nil? contExpression)
    |               preprocessResult
    |               (app addSubProcess contExpression preprocessResult operator)
    |           ))
    |           result
    |       )
    |   )
    |   (def getCalculationResult ( line )
    |       (let
    |           (val expression (app splitNumericAndOperator line))
    |           (val processedValue (if (nil? expression)
    |               "parse error"
    |               nil
    |           ))
    |           processedValue
    |       )
    |   )
    |   (val exitString "exit")
    |   (val invokeCheck "sibal\n")
    |   (defIO runCycle ()
    |       (readline inputString)
    |       (runIO result (if (= inputString exitString)
    |           0
    |           1
    |       ))
    |       (runIO printValue (if result 
    |           (app splitNumericAndOperator inputString)
    |           "parse error"
    |       ))
    |       (runIO crossProcessed (if result
    |           (app crossProcess printValue 1 "*" )
    |           ""
    |       ))
    |       (runIO addSubProcessed (if result
    |           (app addSubProcess crossProcessed 0 "+" )
    |           ""
    |       ))
    |       (print addSubProcessed)
    |       (print (if result "\n" ""))
    |       result
    |   )
    |   (defIO printRunCycle ()
    |       (runIO result (app runCycle))
    |       (runIO nextResult (if result
    |           (app printRunCycle)
    |           result
    |       ))
    |       nextResult
    |   )
    |   (app printRunCycle)
    |)
    |""".stripMargin

    /*
        stringToInt 최적화할 것.
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
    
    */
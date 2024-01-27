import Data.Char

myDigitToInt v | ord v >= 48 && ord v <= 57 = digitToInt v
myDigitToInt v = error (v : " is not character")

asInt_fold :: String -> Int
asInt_fold [] = error "Empty string"
asInt_fold ('-' : []) = error "Invalid string"
asInt_fold ('-' : xs) = (-1) * asInt_fold xs

asInt_fold xs = foldl step 0 xs
    where 
        step acc x = 
            let acc' = acc * 10 + myDigitToInt x
            in if acc' < 0 then error "Number too large" else acc'

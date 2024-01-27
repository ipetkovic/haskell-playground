import Data.Char

safeDigitToInt v | ord v >= 48 && ord v <= 57 = Right (digitToInt v)
safeDigitToInt v = Left (v : " is not character")

asInt_either :: String -> Either String Int
asInt_either [] = Left "Empty string"
asInt_either ('-' : []) = Left "Invalid string"
asInt_either ('-' : xs) = case (asInt_either xs) of
    Left error -> Left error
    Right value -> Right (-1 * value)

asInt_either xs = foldl step (Right 0) xs
    where 
        step acc x =
            let d = safeDigitToInt x 
            in stepInner acc d
            where 
                stepInner (Right acc) (Right d) = 
                    let acc' = acc * 10 + d
                    in if (acc' < 0 ) then Left "Number too large" 
                       else  Right acc' 
                stepInner (Left acc) _ = Left acc
                stepInner acc (Left error) = Left error

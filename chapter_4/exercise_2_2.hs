import Data.Char

asInt_fold :: String -> Int
asInt_fold ('-' : xs) = (-1) * asInt_fold xs
asInt_fold xs = foldl step 0 xs
    where step acc x = acc * 10 + digitToInt x
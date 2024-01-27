import Data.Char

asIntFoldl :: String -> Int
asIntFoldl = foldl step 0
    where step acc x = acc * 10 + digitToInt x

asIntFoldr :: String -> Int
asIntFoldr s = 
    let x = foldr step (1,0) s 
            where step x (f, a) = (f * 10, f * digitToInt x + a)
    in snd x

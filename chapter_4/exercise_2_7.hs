takeWhileRec :: (a -> Bool) -> [a] -> [a]

takeWhileRec p (x:xs) | p x = x : takeWhileRec p xs
takeWhileRec p (x:xs) = []
takeWhileRec p [] = []

takeWhileFoldr :: (a -> Bool) -> [a] -> [a]
takeWhileFoldr p = foldr step [] 
  where step x acc | p x = x : acc
        step x acc = []
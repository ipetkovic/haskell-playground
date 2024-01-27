myGroupBy :: (a -> a -> Bool) -> [a] -> [[a]]
myGroupBy p xs = 
  let ys = drop 1 xs
      ps = zip xs ys
      z = case xs of
        [] -> []
        xs -> [[last xs]]
  in foldr step z ps
    where step (x, y) ((ch : ct) : at) | p x y = (x : ch : ct) : at
          step (x, y) (ah : at) = [x] : ah : at
          step _ [] = []
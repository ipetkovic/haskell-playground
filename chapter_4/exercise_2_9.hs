myGroupBy :: (a -> a -> Bool) -> [a] -> [[a]]
myGroupBy p xs =
  let ys = drop 1 xs
      ps = zip xs ys
      z = case xs of
        [] -> []
        xs -> [[last xs]]
  in foldr mergeStep [] (foldr step z ps)
    where step (x, y) ((ch : ct) : at) | p x y = (x : ch : ct) : at
          step (x, y) (ah : at) = [x] : ah : at
          step (x, y) [] = []

          -- mergeStep :: [a] -> [[a]] -> [[a]]
          mergeStep (x : xs) ((fh : ft) : at) | p x fh = ((x : xs) ++ (fh : ft )): at
          mergeStep xs (ah : at)  = xs : ah : at
          mergeStep xs [] = [xs]
myConcat :: [[a]] -> [a]
myConcat = foldr step []
  where step l acc = l ++ acc
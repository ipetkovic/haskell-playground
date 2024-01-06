mean xs =
    fromIntegral(sumAll xs) / fromIntegral(length xs)
    where sumAll [] = 0
          sumAll (h:t) = h + sumAll t
safeHead :: [a] -> Maybe a
safeTail :: [a] -> Maybe [a]
safeLast :: [a] -> Maybe a
safeInit :: [a] -> Maybe [a]

safeHead [] = Nothing
safeHead (h:t) = Just h

safeTail [] = Nothing
safeTail (h:t) = Just t

safeLast [] = Nothing
safeLast [h] = Just h
safeLast (h:t) = safeLast t

safeInit [] = Nothing
safeInit (h:t) = Just (inner (h:t))
  where inner [h] = []
        inner (h:t) = h : inner t
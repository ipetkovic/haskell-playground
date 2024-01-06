intersperse :: a -> [[a]] -> [a]
intersperse sep [] = []
intersperse sep [h] = h
intersperse sep (h:t) = h ++ (sep : intersperse sep t)
import Data.List (sortBy)

sortListOfLists :: [[a]] -> [[a]]
sortListOfLists =
    sortBy (\ l1 l2 -> compare (length l1) (length l2)) 
        

    
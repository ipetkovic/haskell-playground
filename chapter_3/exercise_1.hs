numberOfElements xs = 
    inner xs 0
    where inner [] count = count
          inner xs count = inner (tail xs) count + 1
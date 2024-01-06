makePalindrome xs = 
    merge xs (invert xs)
    where 
      invert xs = invertInner xs []
        where invertInner [] acc = acc
              invertInner  (h:t) acc = invertInner t (h:acc)
      merge fs ss = invert (mergeInner fs ss []) 
        where mergeInner [] [] acc = acc
              mergeInner [] (sh:st) acc = mergeInner [] st (sh : acc)
              mergeInner (h:t) s acc = mergeInner t s (h : acc)
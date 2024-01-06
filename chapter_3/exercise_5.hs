isPalindrome xs =
    isEqual xs (invert xs)
    where
      invert xs = invertInner xs []
        where invertInner [] acc = acc
              invertInner (h:t) acc = invertInner t (h : acc)
      isEqual [] [] = True
      isEqual (h1:t1) (h2:t2) =
        (h1 == h2) && isEqual t1 t2
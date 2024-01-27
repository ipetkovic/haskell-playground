splitWith :: (a -> Bool) -> [a] -> [[a]]

splitWith p x = reverse (reverseInner (inner p x []))
  where
    inner p [] acc = acc
    inner p (h:t) acc | p h =
      case acc of
        (ah:at) -> inner p t ((h:ah):at)
        [] -> inner p t [[h]]
    inner p (h:t) acc | not (p h) =
      inner p t ([h]:acc)

    reverseInner [] = []
    reverseInner (h:t) =
      reverse h : reverseInner t

    reverse xs = inner xs []
      where
        inner [] acc = acc
        inner (h:t) acc = inner t (h :acc)

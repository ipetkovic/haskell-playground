data BinaryTree a =
    Node {
        v :: a,
        l :: BinaryTree a,
        r :: BinaryTree a
    } |
    Empty

treeHeight :: BinaryTree a -> Int
treeHeight Empty = 0
treeHeight (Node _ l r) = 1 + max (treeHeight l) (treeHeight r)

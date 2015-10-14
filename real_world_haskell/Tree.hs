data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show, Eq, Ord)

height Empty = 0
height (Node a b c) = 1 + max (height b) (height c)

find x Empty = False
find x (Node a b c)
    | x == a    = True
    | x <  a    = find x b
    | x >  a    = find x c

insert x Empty = Node x Empty Empty
insert x (Node a b c)
    | x == a    = Node a b c
    | x <  a    = Node a (insert x b) c
    | x >  a    = Node a b (insert x c)

buildFromList []     = Empty
buildFromList [x]    = Node x Empty Empty
buildFromList (x:xs) = insert x (buildFromList xs)

tree0 = Empty
tree1 = Node 1 (Empty) (Empty)
tree2 = Node 2 (Node 1 Empty Empty) (Empty)
tree3 = Node 2 (Node 1 Empty Empty) (Node 3 Empty Empty)
tree4 = Node 2 (Node 1 Empty Empty) (Node 3 Empty (Node 4 (Empty) (Empty)))
tree5 = Node 2 (Node 1 Empty Empty) (Node 3 Empty (Node 4 Empty (Node 5 Empty Empty)))
tree6 = Node 2 (Node 1 Empty Empty) (Node 3 Empty (Node 4 Empty (Node 6 (Node 5 Empty Empty) Empty)))


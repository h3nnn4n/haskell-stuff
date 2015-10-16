data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show, Eq, Ord)

--height :: (Num a, Ord a) => Tree t -> a
height Empty = 0
height (Node a b c) = 1 + max (height b) (height c)
--
--offset :: (Num a, Ord a) => Tree t -> a
offset Empty = 0
offset (Node a b c) = height b - height c

--checkOffset :: (Num b, Ord b) => Tree a -> [b]
checkOffset Empty        = []
checkOffset (Node a b c) = offset (Node a b c) : checkOffset b ++ checkOffset c

--goL :: Tree a -> Tree a
goL Empty = Empty
goL (Node a b c) = b

--goR :: Tree a -> Tree a
goR Empty = Empty
goR (Node a b c) = c

--find :: Ord a => a -> Tree a -> Bool
find x Empty = False
find x (Node a b c)
    | x == a    = True
    | x <  a    = find x b
    | x >  a    = find x c

--insert :: Ord a => a -> Tree a -> Tree a
insert x Empty = Node x Empty Empty
insert x (Node a b c)
    | x == a    = Node a b c
    | x <  a    = Node a (insert x b) c
    | x >  a    = Node a b (insert x c)

--deleteBiggest :: Tree a -> a, Tree a
deleteBiggest (Node a b Empty) = (a, b)
deleteBiggest (Node a b c) = (w, Node a b wt)
    where (w, wt) = deleteBiggest c

--delete :: Ord a => a -> Tree a -> Tree a
delete x Empty   = Empty
delete x (Node a b c)
    | x <  a     = Node a (delete x b) c
    | x >  a     = Node a b (delete x c)
    | x == a     = deleteFind (Node a b c)
        where deleteFind (Node a' Empty c') = c'
              deleteFind (Node a' b' c')    = Node w wz c'
                where (w, wz) = deleteBiggest b'

--insert :: Ord a => a -> Tree a -> Tree a
insertBalanced x Empty = Node x Empty Empty
insertBalanced x (Node a b c)
    | x == a    = Node a b c
    | x <  a    = avl $ Node a (insertBalanced x b) c
    | x >  a    = avl $ Node a b (insertBalanced x c)

--delete :: Ord a => a -> Tree a -> Tree a
deleteBalanced x Empty   = Empty
deleteBalanced x (Node a b c)
    | x <  a     = avl $ Node a (delete x b) c
    | x >  a     = avl $ Node a b (delete x c)
    | x == a     = deleteFind (Node a b c)
        where deleteFind (Node a' Empty c') = c'
              deleteFind (Node a' b' c')    = avl $ Node w wz c'
                where (w, wz) = deleteBiggest b'

--avl :: Tree a -> Tree a
avl Empty = Empty
avl (Node a l r)
    | abs( offsetNode ) < 2               = Node a l r
    | offsetNode ==  2 && offsetR /= -1   = rotateR (Node a l r)
    | offsetNode ==  2 && offsetR == -1   = rotateR (Node a (rotateL l) r)
    | offsetNode == -2 && offsetR /=  1   = rotateL (Node a l r)
    | offsetNode == -2 && offsetR ==  1   = rotateL (Node a (rotateR l) r)
    where
      offsetNode = offset (Node a l r)
      offsetR    = offset r
      offsetl    = offset l

--rotateR :: Tree a -> Tree a
rotateR Empty = Empty
rotateR (Node a (Node b l r) r')  = Node b l (Node a r r')
rotateR (Node a l r) = Node a l r

--rotateL :: Tree a -> Tree a
rotateL Empty = Empty
rotateL (Node a l (Node b l' r')) = Node a (Node b l l') r'
rotateL (Node a l r) = Node a l r

--buildFromList' :: Ord a => [a] -> Tree a
buildFromList' []     = Empty
buildFromList' [x]    = Node x Empty Empty
buildFromList' (x:xs) = insertBalanced x (buildFromList' xs)

--buildFromList :: Ord a => [a] -> Tree a
buildFromList []     = Empty
buildFromList [x]    = Node x Empty Empty
buildFromList (x:xs) = insert x (buildFromList xs)


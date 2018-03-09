module BST where

data Bst a = Nil | Node a (Bst a) (Bst a)

contains :: (Ord a) => Bst a -> a -> Bool
contains Nil _ = False
contains (Node root left right) y
  | y == root = True
  | y < root = contains left y
  | y > root = contains right y

add :: (Ord a) => Bst a -> a -> (Bst a, Bool)
add Nil x = (Node x Nil Nil, True)
add n@(Node x left right) y
  | x == y = (n, False)
  | x > y && not la = (n, False)
  | x > y = (Node x lp right, True)
  | x < y && not ra = (n, False)
  | x < y = (Node x left rp, True)
  where
    (lp, la) = add left y
    (rp, ra) = add right y

addIgn :: (Ord a) => Bst a -> a -> Bst a
addIgn tree el = fst $ add tree el

-- Second argument is true if element was deleted
remove :: (Ord a) => Bst a -> a -> (Bst a, Bool)
remove Nil _ = (Nil, False)
remove n@(Node root left right) x
  | x == root =
    case largestElement left of
      Just newRoot -> (Node newRoot (fst $ remove left newRoot) right, True)
      Nothing -> (right, True)
  | x < root =
    let (newLeft, removalResult) = remove left x in 
      (Node root newLeft right, removalResult)
  | x > root =
    let (newRight, removalResult) = remove right x in
      (Node root left newRight, removalResult)

largestElement :: (Ord a) => Bst a -> Maybe a
largestElement Nil = Nothing
largestElement (Node root _ Nil) = Just root
largestElement (Node root _ right) = largestElement right

size :: (Ord a) => Bst a -> Int
-- size root@(Node x left right) = 1 + (size left) + (size right)
size tree = sizeRec tree 0
  where
    sizeRec Nil sum = sum
    sizeRec (Node x left right) sum = (sizeRec (newSum `seq` left) newSum) + (sizeRec right 0)
      where
        newSum = sum + 1
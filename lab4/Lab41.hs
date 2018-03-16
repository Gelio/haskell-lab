module Lab41 where

data Alfabet = A | B | C
data Trie = Nil | Node Bool Trie Trie Trie deriving Eq

-- Trie from task
simpleTrie = Node False
  Nil
  (Node False
    Nil
    (Node True Nil Nil Nil)
    (Node True Nil Nil Nil)
  )
  (Node True
    (Node True Nil Nil Nil) 
    Nil
    (Node True Nil Nil Nil)
  )

add :: Trie -> [Alfabet] -> (Trie, Bool)
add Nil [] = (Node True Nil Nil Nil, True)
add Nil word@(x:xs) = add (Node False Nil Nil Nil) word
add n@(Node exists a b c) [] = if exists
  then (n, False)
  else (Node True a b c, True)
add (Node exists a b c) (x:xs) = case x of
  A -> (Node exists added b c, wasAdded)
    where (added, wasAdded) = add a xs
  B -> (Node exists a added c, wasAdded)
    where (added, wasAdded) = add b xs
  C -> (Node exists a b added, wasAdded)
    where (added, wasAdded) = add c xs

simpleTrie' = fst $ add (fst $ add Nil [C]) [B, C]

contains :: Trie -> [Alfabet] -> Bool
contains Nil _ = False
contains (Node True _ _ _) [] = True
contains (Node False _ _ _) [] = False
contains (Node _ a b c) (x:xs) =
  case x of
    A -> contains a xs
    B -> contains b xs
    C -> contains c xs

instance Show Trie where
  show trie = show $ listWords trie ""
    where 
      listWords :: Trie -> String -> [String]
      listWords Nil _ = []
      listWords (Node exists a b c) prefix = 
        if exists
          then prefix:rest
          else rest
        where
          rest = (listWords a (prefix ++ "A")) ++ (listWords b (prefix ++ "B")) ++ (listWords c (prefix ++ "C"))

nieskonczoneDrzewo :: Trie
nieskonczoneDrzewo = Node False duzoA jednoB Nil
  where
    duzoA = Node True duzoA jednoB Nil 
    jednoB = Node True Nil Nil Nil
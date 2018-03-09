module SortedList (SortedList, createSortedList, add, contains, remove, SortedList.length)
  where

-- List will be sorted in ascending order (smallest first)
data SortedList a = Head a (SortedList a) | EmptyList

createSortedList :: (Ord a) => a -> SortedList a
createSortedList x = Head x EmptyList

getHead :: (Ord a) => SortedList a -> Maybe a
getHead EmptyList = Nothing
getHead (Head x _) = Just x


add :: (Ord a) => SortedList a -> a -> SortedList a
add EmptyList x = createSortedList x
add list@(Head head tail) x
  | x <= head = Head x list
  | otherwise = Head head (Head x tail)

contains :: (Ord a) => SortedList a -> a -> Bool
contains EmptyList _ = False
contains (Head head tail) x
  | x < head = False
  | x == head = True
  | otherwise = contains tail x

remove :: (Ord a) => SortedList a -> a -> SortedList a
remove EmptyList _ = EmptyList
remove list@(Head head tail) x
  | x < head = list
  | x == head = tail
  | otherwise = Head head $ remove tail x

length :: (Ord a) => SortedList a -> Int
length EmptyList = 0
length list@(Head _ _) = lengthRec list 0
  where
    lengthRec EmptyList sum = sum
    lengthRec (Head _ tail) sum = lengthRec (seq sum2 tail) sum2
      where sum2 = sum + 1

average :: SortedList Double -> Double
average EmptyList = 0
average list@(Head _ _) = averageRec list 0 0
  where 
    averageRec EmptyList count sum = sum / count
    averageRec (Head x tail) count sum = averageRec (newCount `seq` newSum `seq` tail) newCount newSum
      where
        newCount = count + 1
        newSum = sum + x

filterList :: (Ord a) => (a -> Bool) -> SortedList a -> SortedList a
filterList f EmptyList = EmptyList
filterList f (Head x tail)
  | f x = Head x $ filteredTail
  | otherwise = filteredTail
  where
    filteredTail = filterList f tail
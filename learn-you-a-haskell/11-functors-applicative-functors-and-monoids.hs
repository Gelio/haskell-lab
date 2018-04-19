import Control.Applicative
import Data.Monoid

-- We can use the following type to simulate our own list
data List a = Empty | Value a (List a) deriving (Show)

-- Make the list a Functor

instance Functor List where
  fmap _ Empty = Empty
  fmap f (Value a l) = Value (f a) (fmap f l)

-- Write a function which appends one list on to another
combineLists:: List a -> List a -> List a
combineLists Empty b = b
combineLists (Value a l) b = Value a $ combineLists l b

-- Make our list a Monoid
instance Monoid (List a) where
  mempty = Empty
  mappend = combineLists

-- Make our list an Applicative
-- Behaves like a ZipList, not an nondeterministic list
instance Applicative List where
  pure x = Value x Empty

  Empty <*> _ = Empty
  _ <*> Empty = Empty
  Value f fs <*> Value x xs = Value (f x) (fs <*> xs)

-- Make sure that the List obeys the laws for Applicative and Monoid

-- Create some lists of numbers of different lengths such as:
twoValueList = Value 10 $ Value 20 Empty

-- Use <$> on the lists with a single-parameter function, such as:
plusTwo = (+2)
mappedTwoValueList = plusTwo <$> twoValueList

-- Use <$> and <*> on the lists with a binary function
multipliedTwoValueList = (*) <$> twoValueList <*> twoValueList

-- Create some lists of binary functions
binaryFunctions = Value (*) $ Value (-) $ Value (+) Empty

-- Use <*> on the binary functions list and the number lists
binaryMappedTwoValueList = binaryFunctions <*> mappedTwoValueList <*> mappedTwoValueList

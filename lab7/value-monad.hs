module ValueMonad where

data Value a = Value a deriving (Show, Ord, Eq)

instance Functor Value where
  fmap f (Value x) = Value $ f x

instance Applicative Value where
  pure = Value
  (Value f) <*> (Value x) = Value $ f x

instance Monad Value where
  (Value x) >>= f = f x

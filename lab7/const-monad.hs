module ConstMonad where

-- Inspired by https://stackoverflow.com/questions/11530412/monad-for-const

data Const' a b = Const' a b

getConst :: Const' a b -> a
getConst (Const' x _) = x

instance Functor (Const' m) where
  fmap f (Const' v x) = Const' v $ f x

instance Monoid m => Applicative (Const' m) where
  pure x = Const' mempty x
  (Const' x f) <*> (Const' y z) = Const' (x `mappend` y) $ f z

instance Monoid m => Monad (Const' m) where
  (Const' x y) >>= f = case f y of
    Const' _ yRes -> Const' x yRes

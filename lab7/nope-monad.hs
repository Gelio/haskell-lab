module NopeMonad where

data Nope a = Nope deriving (Show)

instance Functor Nope where
  fmap f Nope = Nope

instance Applicative Nope where
  pure _ = Nope
  Nope <*> Nope = Nope

instance Monad Nope where
  Nope >>= f = Nope

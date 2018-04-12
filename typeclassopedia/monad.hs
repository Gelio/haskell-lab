module Monad where

-- http://blog.sigfpe.com/2007/04/trivial-monad.html
g :: Int -> Maybe Int -> Maybe Int
g x y = y >>= return . (x+)

h :: Maybe Int -> Maybe Int -> Maybe Int
h x y = (+) <$> x <*> y

h' :: Maybe Int -> Maybe Int -> Maybe Int
h' x y = x >>= \xRes -> y >>= Just . (xRes+)

join :: Maybe (Maybe Int) -> Maybe Int
join x = x >>= id

-- instance Monad [] where
--   return x = [x]

--   [] >>= _ = []
--   xs >>= f = concatMap f xs

data Free f a = Var a | Node (f (Free f a))

instance Functor f => Functor (Free f) where
  fmap f (Var a) = Var (f a)
  fmap f (Node x) = Node $ fmap (fmap f) x

-- x >>= f = join $ (fmap f) x
-- x >>= f = join $ liftM f x


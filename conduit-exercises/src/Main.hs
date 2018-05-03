{-# LANGUAGE ExtendedDefaultRules #-}

module Main where
import Conduit

-- 1. Custom iterMC
-- iterMC' :: Monad m => (i -> m ()) -> ConduitT i i m ()
-- iterMC' f = mapMC (\x -> f x >> return x)

-- main :: IO ()
-- main = do
--     res <- runConduit $ yieldMany [1..10] .| iterMC' print .| sumC
--     print res

-- 2. sink without `do` notation
-- sink :: Monad m => ConduitT Int o m (String, Int)
-- sink = do
--   x <- takeC 5 .| mapC show .| foldC
--   y <- sumC
--   return (x, y)

-- -- Rewrite of do notation
-- sink' :: Monad m => ConduitT Int o m (String, Int)
-- sink' = takeC 5 .| mapC show .| foldC >>= \x ->
--         sumC >>= \y ->
--         return (x, y)

-- -- Applicative
-- sink'' :: Monad m => ConduitT Int o m (String, Int)
-- sink'' = (,) <$> (takeC 5 .| mapC show .| foldC) <*> sumC

-- main :: IO ()
-- main = do
--     let res = runConduitPure $ yieldMany [1..10] .| sink''
--     print res

-- 3. Perform different operations for each item
-- trans :: Monad m => ConduitT Int Int m ()
-- trans = do
--   takeC 5 .| mapC (+ 1)
--   mapC (* 2)

-- trans' :: Monad m => ConduitT Int Int m ()
-- trans' = do
--   takeC 3 .| mapC (+1)
--   takeC 3 .| mapC (*2)
--   takeC 3 .| mapC (\x -> x - 10)
--   sinkNull

-- main :: IO ()
-- main = runConduit $ yieldMany [1..10] .| trans' .| mapM_C print

-- 4. yieldMany using yield
-- yieldMany' :: Monad m => [a] -> ConduitT i a m ()
-- yieldMany' = mapM_ yield

-- main :: IO ()
-- main = runConduit $ yieldMany' [1,2]  .| mapM_C print

-- 5. filterC and mapMC
-- import Control.Monad

-- filterC' :: Monad m => (a -> Bool) -> ConduitT a a m ()
-- filterC' f = do
--   mx <- await
--   case mx of
--     Nothing -> return ()
--     Just x -> when (f x) (yield x) >> filterC' f

-- mapMC' :: Monad m => (a -> m b) -> ConduitT a b m ()
-- mapMC' f = do
--   mx <- await
--   case mx of
--     Nothing -> return ()
--     Just x -> lift (f x) >>= yield >> mapMC' f

-- main :: IO ()
-- main = runConduit $ yieldMany [1..10] .| filterC (<5) .| mapMC' (\x -> print x >> return x) .| sinkNull

-- 6. peek
import Control.Monad
import Data.Maybe

peek' :: Monad m => ConduitT a o m (Maybe a)
peek' = do
  mx <- await
  case mx of
    Nothing -> return Nothing
    Just x -> leftover x >> return (Just x)

peek'' :: Monad m => ConduitT a o m (Maybe a)
peek'' = await >>= maybe (return Nothing) (\x -> leftover x >> return (Just x))

main :: IO ()
main = runConduit $ yieldMany [1..10] .| do
  x <- peek'
  lift $ maybe (return ()) (\x' -> when (x' > 5) (void (print x))) x
  sinkNull

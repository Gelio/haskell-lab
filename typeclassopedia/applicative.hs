module Applicative where

-- instance Applicative Maybe where
--   pure :: a -> Maybe a
--   pure = Just

--   (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
--   Nothing <*> _ = Nothing
--   _ <*> Nothing = Nothing
--   (Just f) <*> (Just x) = Just $ f x

sequenceAL :: Applicative f => [f a] -> f [a]
sequenceAL [] = pure []
sequenceAL (x:xs) = (:) <$> x <*> sequenceAL xs

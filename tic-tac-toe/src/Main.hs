module Main where

import Data.Sequence as S
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict

data Token = X | O deriving (Show)

type Board = S.Seq (Maybe Token)
type TTTState a = StateT Board IO a

emptyBoard :: Board
emptyBoard = S.replicate 9 Nothing

putToken :: Token -> Int -> TTTState Bool
putToken t pos
  | pos < 1 || pos > 9 = return False
  | otherwise = modify' (S.update (pos-1) (Just t)) >> return True

displayBoard :: TTTState ()
displayBoard = do
  board <- get
  lift $ print board


main :: IO ()
main = void $ execStateT (do
    result <- putToken X 1
    if result then
      lift $ putStrLn "Yes"
    else
      lift $ putStrLn "No"
    displayBoard
    return ()
  ) emptyBoard

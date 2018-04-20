module Main where

import qualified Data.Sequence as S
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Text.Read (readMaybe)

data Token = X | O deriving (Show)

type Board = S.Seq (Maybe Token)
type TTTState a = StateT Board IO a

emptyBoard :: Board
emptyBoard = S.replicate 9 Nothing

putToken :: Token -> Int -> TTTState Bool
putToken t pos
  | pos < 1 || pos > 9 = return False
  | otherwise = modify' (S.update (pos-1) (Just t)) >> return True

winningIndices = [[0, 1, 2], [0, 3, 6], [0, 4, 8], [1, 4, 7], [2, 5, 8], [3, 4, 5], [6, 7, 8], [2, 4, 6]]

checkWinningIndexSet :: Board -> [Int] -> Maybe Token
checkWinningIndexSet board indices = sequence tokens >>= \t -> if all (== head t) t then return $ head t else Nothing
  where
    tokens :: [Maybe Token]
    tokens = fmap indices (`S.lookup` board)

isWin :: TTTState (Maybe Token)
isWin = do
  board <- get
  let boardWins = filter (checkWinningIndexSet board) winningIndices
    in
      case boardWins of
        [] -> return Nothing
        (x:xs) -> return x

displayBoard :: TTTState ()
displayBoard = do
  board <- get
  lift $ print board

ask :: String -> IO String
ask s = putStrLn s >> getLine

readInt :: String -> Maybe Int
readInt = readMaybe

playGame :: TTTState ()
playGame = do
  index <- lift $ ask "Index between 1 and 9"
  maybe (lift $ putStrLn "Index should be a number") (\i -> putToken X i >> displayBoard >> isWin >>= \w -> when w (lift $ putStrLn "Win!")) (readInt index)
  playGame

main :: IO ()
main = void $ execStateT playGame emptyBoard

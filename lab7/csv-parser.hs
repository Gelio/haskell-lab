module CSVParser where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Maybe (fromJust)
import Text.Read (readMaybe)

data Parser a = Parser { parse :: String -> Maybe (a, String) }

run :: Parser a -> String -> Maybe a
run (Parser p) input = fmap fst (p input)

runEnsureConsumeAll :: Parser a -> String -> Maybe a
runEnsureConsumeAll (Parser p) input = case p input of
  Just (v, "") -> Just v
  _ -> Nothing

readChar :: Parser Char
readChar = Parser fun
  where
    fun [] = Nothing
    fun (c:cs) = Just (c, cs)

failure :: Parser a
failure = Parser $ const Nothing

instance Functor Parser where
  fmap f (Parser pf) = Parser $ (fmap (\(x, s) -> (f x, s))) . pf

instance Applicative Parser where
  pure x = Parser (\s -> Just (x, s))
  (Parser f1) <*> (Parser f2) = Parser $ \s -> do
    (fun, s1) <- f1 s
    (arg, s2) <- f2 s1
    return (fun arg, s2)

instance Monad Parser where
  Parser f >>= fun = Parser $ \s -> do
    (arg, s1) <- f s;
    (parse $ fun arg) s1

  fail _ = failure

instance Alternative Parser where
  empty = failure
  p1 <|> p2 = Parser $ \s -> (parse p1 s <|> parse p2 s)

ensureChar :: Char -> Parser Char
ensureChar c = readChar >>= (\d -> if c == d then return d else failure)

ensureMatching :: String -> Parser Char
ensureMatching set = readChar >>= (\d -> if d `elem` set  then return d else failure)

readMatching :: String -> Parser String
readMatching set = some $ ensureMatching set

eatChars :: String -> Parser ()
eatChars set = void $ many $ ensureMatching set

digits = "0123456789"

readInt :: Parser Integer
readInt = fmap (fromJust . readMaybe) $ readMatching digits

readDouble :: Parser Double
readDouble = do
  integer <- readMatching digits
  decimal <- (ensureChar '.' >> readMatching digits >>= return . ('.':)) <|> return ""
  return $ fromJust $ readMaybe (integer ++ decimal)

readDoubleList :: Parser [Double]
readDoubleList = do
  n <- readDouble
  rest <- (ensureChar ',' >> readDoubleList) <|> return []
  return (n:rest)

readDoubleListRows :: Parser [[Double]]
readDoubleListRows = do
  row <- readDoubleList
  rest <- (ensureChar '\n' >> readDoubleListRows) <|> return []
  return (row:rest)

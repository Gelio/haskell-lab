{-# OPTIONS -Wall #-}
module Parser (readJSONVal, readJSONFile, run, runEnsureConsumeAll, parse) where

import Control.Applicative
import Control.Monad
import qualified Data.Map as M
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

ensureString :: String -> Parser String
ensureString "" = return ""
ensureString (c:cs) = ensureChar c >> fmap (c:) (ensureString cs)

readUntil :: Char -> Parser String
readUntil delim = do
  c <- readChar
  if c == delim then
    return ""
  else
    (c:) <$> readUntil delim

eatChars :: String -> Parser ()
eatChars set = void $ many $ ensureMatching set

parseInt :: String -> Integer
parseInt l = parseRec 0 l
  where
    parseRec :: Integer -> String -> Integer
    parseRec sum (x:xs) = (parseRec $! ((toInteger $ digitToInt x)+10*sum)) xs
    parseRec sum [] = sum

digits :: String
digits = "0123456789"

readInt :: Parser Integer
readInt = fmap parseInt $ readMatching digits

readDouble :: Parser Double
readDouble = do
  integer <- readMatching digits
  decimal <- (ensureChar '.' >> readMatching digits >>= return . ('.':)) <|> return ""
  return $ fromJust $ readMaybe (integer ++ decimal)

readString :: Parser String
readString = ensureChar '"' >> readUntil '"'

readString' :: Parser String
readString' = do
  _ <- ensureChar '"'
  inside <- readStringInside
  return inside

  where
    readStringInside = do
      c <- readChar
      if c == '\\' then
        do
          anotherC <- readChar
          if anotherC == '"' then
            readStringInside >>= \rest -> return ('"':rest)
          else
            readStringInside
      else if c == '"' then
        return ""
      else
        readStringInside >>= \rest -> return (c:rest)


readBool :: Parser Bool
readBool = readTrue <|> readFalse
  where
    readTrue = ensureString "true" >> return True
    readFalse = ensureString "false" >> return False

eatWhitespace :: Parser ()
eatWhitespace = eatChars "\n\t "

type ObjectMap = M.Map String JSONVal
data JSONVal = DoubleVal Double | StringVal String | BoolVal Bool | ArrayVal [JSONVal] | NullVal | ObjectVal ObjectMap deriving (Show)

readDoubleVal :: Parser JSONVal
readDoubleVal = DoubleVal <$> readDouble

readStringVal :: Parser JSONVal
readStringVal = StringVal <$> readString'

readBoolVal :: Parser JSONVal
readBoolVal = BoolVal <$> readBool

readNullVal :: Parser JSONVal
readNullVal = ensureString "null" >> return NullVal

readArrayVal :: Parser JSONVal
readArrayVal = do
  _ <- ensureChar '['
  arrayItems <- readArrayItems
  eatWhitespace
  _ <- ensureChar ']'
  return $ ArrayVal arrayItems
  where
    readArrayItems :: Parser [JSONVal]
    readArrayItems = do
      eatWhitespace
      item <- readJSONVal
      eatWhitespace
      rest <- (ensureChar ',' >> readArrayItems) <|> return []
      return (item:rest)

readObjectVal :: Parser JSONVal
readObjectVal = do
  _ <- ensureChar '{'
  objectMap <- readObjectVal' M.empty
  _ <- ensureChar '}'
  return objectMap

  where
    readObjectVal' :: ObjectMap -> Parser JSONVal
    readObjectVal' m = do
      eatWhitespace
      key <- readString'
      eatWhitespace
      _ <- ensureChar ':'
      eatWhitespace
      value <- readJSONVal
      eatWhitespace

      let updatedMap = M.insert key value m in
        (ensureChar ',' >> readObjectVal' updatedMap) <|> return (ObjectVal updatedMap)


readJSONVal :: Parser JSONVal
readJSONVal = readDoubleVal <|> readStringVal <|> readBoolVal <|> readNullVal <|> readArrayVal <|> readObjectVal

readJSONFile :: FilePath -> IO (Maybe JSONVal)
readJSONFile path = readFile path >>= return . run readJSONVal

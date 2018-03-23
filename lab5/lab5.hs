import Text.Read
import Control.Applicative


trySum :: String -> String -> Maybe Int
trySum x y = do
  a <- readMaybe x
  b <- readMaybe y
  return (a+b)


trySum' :: String -> String -> Maybe Int
trySum' x y = do
  a <- readMaybe x
  do 
    b <- readMaybe y
    return (a+b)

trySum'' :: String -> String -> Maybe Int
trySum'' x y = readMaybe x >>= (\a -> readMaybe y >>= (return . (a+)))

trySum'''  :: String -> String -> Maybe Int
trySum''' x y = (+) <$> readMaybe x <*> readMaybe y

ask :: String -> IO String
ask prompt = putStrLn prompt >> getLine

ask' :: String -> IO String
ask' prompt = do
  putStrLn prompt
  getLine
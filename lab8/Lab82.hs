module Main where

import System.IO
import System.IO.Error
import System.Environment
import Text.Read (readMaybe)

-- Funkcje pomocnicze

selectByte :: Handle -> Integer -> IO ()
selectByte h pos = hSeek h AbsoluteSeek pos >> hGetChar h >>= putChar

selectBytes :: Handle -> [Integer] -> IO ()
selectBytes h = mapM_ (selectByte h)

selectFromFile :: FilePath -> [Integer] -> IO ()
selectFromFile path bytes = withFile path ReadMode (flip selectBytes bytes)

stringsToIntegers :: [String] -> Maybe [Integer]
stringsToIntegers = mapM readMaybe

-- Główny program

catchErrors :: IO () -> IO ()
catchErrors op = catchIOError op errorHandler
  where
    errorHandler :: IOError -> IO ()
    errorHandler e
      | isPermissionError e = putStrLn "Nie można odczytać pliku (brak uprawnień)"
      | isDoesNotExistError e = putStrLn "Plik nie istnieje"
      | otherwise = putStrLn "Nie można odczytać pliku" >> ioError e


parseArgs :: [String] -> IO ()
parseArgs [] = putStrLn "Za mało argumentów"
parseArgs (fileName:bytes) = maybe parseFailed (catchErrors . selectFromFile fileName) (stringsToIntegers bytes)
  where
    parseFailed = putStrLn "Argumenty (oprócz pierwszego) powinny być liczbami"

main :: IO ()
main = getArgs >>= parseArgs

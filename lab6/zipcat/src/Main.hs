{-# OPTIONS -Wall #-}
module Main where

import System.Environment
import System.IO
import System.IO.Error

main :: IO ()
main = catchIOError
  (do
      (output:inputs) <- getArgs
      outHandle <- openFile output WriteMode
      fHandles <- mapM (`openFile` ReadMode) inputs
      processFiles outHandle fHandles
      hClose outHandle
      mapM_ hClose fHandles
  ) (\e ->
        if isPermissionError e
        then
          putStrLn "Insufficient permissions"
        else if isDoesNotExistError e then
          putStrLn "Input file does not exist"
        else
          ioError e
  )


processFiles :: Handle -> [Handle] -> IO ()
processFiles output inputs = catchIOError
    (do
      chars <- mapM hGetChar inputs
      hPutStr output chars
      processFiles output inputs
    )
    (\e -> if isEOFError e then return () else ioError e)

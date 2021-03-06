module Main where

import Data.List
-- import qualified aby uniknąć konfliktów
-- nazw funkcji w List i Map
import qualified Data.Map.Strict as M
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import System.Environment
import System.IO.Error


-- aliasy typów do poprawy czytelności
type Phone = String
type Name = String
-- i skrócenia zapisu
type StIO a = StateT (M.Map Name Phone) IO a


-- funkcje pomocnicze operujące na zapisanych numerach
storePhone :: Name -> Phone -> StIO ()
storePhone name phone = modify' (M.insert name phone)

findByPrefix :: Name -> StIO [(Name, Phone)]
findByPrefix prefix =
  fmap (filter (\(n,_) -> isPrefixOf prefix n) . M.toAscList)  get

removePerson :: Name -> StIO ()
removePerson name = modify' (M.delete name)


ask :: String -> StIO String
ask prompt = lift (putStrLn prompt >> getLine)

storePhoneCommand :: StIO ()
storePhoneCommand = do
  name <- ask "Name?"
  phone <- ask "Phone?"
  storePhone name phone

findPhoneCommand :: StIO ()
findPhoneCommand = ask "Name prefix?" >>= findByPrefix >>= lift . print

deletePhoneCommand :: StIO ()
deletePhoneCommand = ask "Name?" >>= removePerson

commands :: M.Map String (StIO Bool)
commands = M.fromList [
  ("add", storePhoneCommand >> return True),
  ("find", findPhoneCommand >> return True),
  ("remove", deletePhoneCommand >> return True),
  ("exit", return False)
 ]

unknownCommand :: StIO Bool
unknownCommand = lift (putStrLn "Unknown command") >> return True

processCommand :: String -> StIO Bool
processCommand cmd = M.findWithDefault unknownCommand cmd commands

readCommand :: StIO Bool
readCommand = lift getLine >>= processCommand

mainLoop :: StIO ()
mainLoop = do
  result <- readCommand
  when result mainLoop

main :: IO ()
main = do
  (file:_) <- getArgs
  initialMap <- catchIOError (do
        m <- read <$!> readFile file
        print m
        return m
    ) (\ex -> if isDoesNotExistError ex then return M.empty else ioError ex)

  finalMap <- execStateT mainLoop initialMap
  writeFile file $ show finalMap

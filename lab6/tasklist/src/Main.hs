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
import Text.Read(readMaybe)
import qualified Data.PSQueue as P


-- aliasy typów do poprawy czytelności
type TaskName = String
type Priority = Int
-- i skrócenia zapisu
type PQueue = P.PSQ TaskName Priority
type StIO a = StateT PQueue IO a


addTask :: TaskName -> Priority -> StIO ()
addTask name priority = modify' (P.insert name priority)

getTask :: StIO (Maybe TaskName)
getTask = gets (\q -> P.key <$> P.findMin q)

ask :: String -> StIO String
ask prompt = lift (putStrLn prompt >> getLine)

readInt :: String -> Maybe Int
readInt = readMaybe

readPriority :: String -> Maybe Priority
readPriority = readInt

putStrLnStIO :: String -> StIO ()
putStrLnStIO = lift . putStrLn

addTaskCommand :: StIO ()
addTaskCommand = do
  name <- ask "Task name?"
  priority <- ask "Priority?"
  maybe (putStrLnStIO "The priority must be an integer") (\p -> addTask name p >> putStrLnStIO "Task added") (readPriority priority)

displayEmptyQueueMessage :: StIO ()
displayEmptyQueueMessage = putStrLnStIO "The tasklist is empty"

printTaskName :: TaskName -> StIO ()
printTaskName taskName = putStrLnStIO $ "Top task: " ++ taskName

peekCommand :: StIO ()
peekCommand = getTask >>= maybe displayEmptyQueueMessage printTaskName

removeTopTask :: StIO ()
removeTopTask = modify' P.deleteMin

popCommand :: StIO Bool
popCommand = getTask >>= maybe (displayEmptyQueueMessage >> return False) (\taskName -> removeTopTask >> printTaskName taskName >> return True)

commands :: M.Map String (StIO Bool)
commands = M.fromList [
  ("add", addTaskCommand >> return True),
  ("peek", peekCommand >> return True),
  ("pop", popCommand),
  ("exit", return False)
  ]

unknownCommand :: StIO Bool
unknownCommand = (lift $ putStrLn "Unknown command") >> return True

processCommand :: String -> StIO Bool
processCommand cmd = M.findWithDefault unknownCommand cmd commands

readCommand :: StIO Bool
readCommand = lift getLine >>= processCommand

mainLoop :: StIO ()
mainLoop = do
  result <- readCommand
  if result
    then mainLoop
    else return ()

main :: IO ()
main = do
  execStateT mainLoop P.empty
  return ()

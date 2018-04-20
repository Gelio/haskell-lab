module RPNCalc where

import Control.Monad
import Text.Read
import Control.Applicative

-- Kod z zadania
data RPNCommand = Push Double | UnOp (Double -> Double) | BinOp (Double -> Double -> Double)

parseUnOp :: String -> Maybe RPNCommand
parseUnOp "sqrt" = Just $ UnOp sqrt
parseUnOp "log" = Just $ UnOp log
parseUnOp _ = Nothing

parseBinOp :: String -> Maybe RPNCommand
parseBinOp "+" = Just $ BinOp (+)
parseBinOp "*" = Just $ BinOp (*)
parseBinOp "-" = Just $ BinOp (-)
parseBinOp _ = Nothing

rpnOperation :: [Double] -> RPNCommand -> Maybe [Double]
rpnOperation s (Push x)  = Just $ x:s
rpnOperation (s:ss) (UnOp op) = Just $ (op s):ss
rpnOperation _ (UnOp _) = Nothing
rpnOperation (s1:s2:ss) (BinOp op) = Just $ (s1 `op` s2):ss
rpnOperation _ (BinOp _) = Nothing

-- Rozwiązanie poniżej
readDouble :: String -> Maybe Double
readDouble = readMaybe

parseDouble :: String -> Maybe RPNCommand
parseDouble = fmap Push . readDouble

parseRpnCommand :: String -> Maybe RPNCommand
parseRpnCommand s = parseUnOp s <|> parseBinOp s <|> parseDouble s

rpnStringOperation :: [Double] -> String -> Maybe [Double]
rpnStringOperation stack s = parseRpnCommand s >>= rpnOperation stack

runRpn :: [String] -> Maybe [Double]
runRpn = foldM rpnStringOperation []

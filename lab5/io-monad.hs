import System.Environment

-- singleCat :: [String] -> IO ()
-- singleCat [] = return ()
-- singleCat (x:xs) = readFile x >>= putStr >> singleCat xs

-- main :: IO ()
-- main = do
--   args <- getArgs
--   singleCat args

main :: IO ()
main = getArgs >>= mapM_ (\file -> readFile file >>= putStr)
main = do
  putStrLn "Oto wszystkie pary liczb, gdzie 10>=a>b>=0"
  putStrLn $ show $ do
    a <- [0..10]
    b <- filter (a>) [0..10]
    return (a,b)
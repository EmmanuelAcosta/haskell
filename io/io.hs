read2lines :: IO ()
read2lines =
  do
    getLine
    getLine
    putStrLn "Two lines read."

put4times :: String -> IO ()
put4times str = do putStrLn str
                   putStrLn str
                   putStrLn str
                   putStrLn str
getNput :: IO ()
getNput = do line <- getLine
             putStrLn line

reverse2lines :: IO ()
reverse2lines = do line1 <- getLine
                   line2 <- getLine
                   let rev1 = reverse line1
                   let rev2 = reverse line2
                   putStrLn rev2
                   putStrLn rev1


getInt :: IO Integer
getInt = do line <- getLine
            return (read line :: Integer)

main :: IO ()
main =
  do
    read2lines
    read2lines
    put4times "Hello"
    put4times "World"
    getNput
    reverse2lines
    n <- getInt
    print n
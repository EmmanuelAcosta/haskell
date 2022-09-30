copy :: IO ()
copy = do 
    line <- getLine
    if line == "quit"
        then return ()
        else do
            putStrLn line
            copy

copyN :: Integer -> IO ()
copyN n =
    if n <= 0
    then return ()
    else do line <- getLine
            putStrLn line
            copyN (n-1)

copyEmpty :: IO ()
copyEmpty =
    do line <- getLine
       if line == ""
       then return ()
       else do putStrLn line
               copyEmpty

copyCount :: Integer -> IO ()
copyCount n =
    do line <- getLine
       if line == ""
       then putStrLn (show n ++ " lines copied.")
       else do putStrLn line
               copyCount (n+1)
main :: IO ()
main = do 
          copyCount 0

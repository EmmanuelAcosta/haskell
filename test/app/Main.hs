module Main where

f :: Integer -> Integer -> Integer
f m n
  | m < n     = m
  | otherwise = f (m - n) n
fun :: Int -> Int -> Int
fun m n = fromIntegral (f (toInteger m) (toInteger n))

main :: IO ()
main = do 
    print $ f 10 3
    print $ f 10 5
    print $ f 10 7
    print $ fun(1 2)

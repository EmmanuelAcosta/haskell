{- module Main where

allEqual :: Eq a => [a] -> Bool
allEqual [] = error "Empty list"
allEqual [x] = error "Singleton list"
allEqual [x, y] = x == y
allEqual (x:y:xs:xys) = x == y && allEqual (y:xs:xys)




main :: IO ()
main = do
            print $ allEqual [1,2,3] -- False
            print $ allEqual [1,1,1] -- True
            print $ allEqual [1] -- Error
            --print $ allEqual [] -- Error -}


module Main where

allEqual :: Ord a => Num a => [a] -> Bool
allEqual [] = error "Empty list"
allEqual [x] = error "Singleton List"
allEqual [x,y] = x==y
allEqual (x:y:xs:xys) = x == y && allEqual (y:xs:xys)

main :: IO ()
main = do
            print $ allEqual [] -- error
            print $ allEqual [1,2,3] -- False
            print $ allEqual [1,1,1] -- True
            print $ allEqual [1] -- Error
             



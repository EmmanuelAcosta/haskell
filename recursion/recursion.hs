maximum' :: (Ord a) => [a] -> a
maximum' [] = error "Máximo de una lista vacía"
maximum' [x] = x
maximum' (x:xs)
    | x > maxTail = x
    | otherwise   = maxTail
    where maxTail = maximum' xs
maximum2' :: (Ord a) => [a] -> a
maximum2' []     = error "maximum of empty list"
maximum2' [x]    = x
maximum2' (x:xs) = x `max` maximum' xs

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0    = []
    | otherwise = x:replicate' (n-1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0   = []
take' _ []     = []
take' n (x:xs) = x : take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted  = quicksort [a | a <- xs, a > x]
    in  smallerSorted ++ [x] ++ biggerSorted

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x       = x : filter' p xs
    | otherwise = filter' p xs

largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000,99999..])
    where p x = x `mod` 3829 == 0

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
    | even n =  n:chain (n `div` 2)
    | odd n  =  n:chain (n*3 + 1)

{- numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
    where isLong xs = length xs > 15 -}
numLongChains :: Int
numLongChains = length (filter (\xs -> length xs > 15) (map chain [1..100]))

map2' :: (a -> b) -> [a] -> [b]
map2' f xs = foldr (\x acc -> f x : acc) [] xs

reverse2' :: [a] -> [a]
reverse2' = foldl (\acc x -> x : acc) []

main :: IO ()
main = do
    print (maximum' [2,1,4,3,55])
    print (replicate' 3 2)
    print (take' 2 [3,2,1,3,4])
    print (reverse' [1,2,3,1,2,3,4,4,4,4,5,5])
    print (reverse' [1])
    print (zip' [3,2] [3,211,2,4])
    print (quicksort "el veloz murcielago hindu comia feliz cardillo y kiwi")
    print (zipWith' (++) ["foo ", "bar ", "baz "] ["fighters", "hoppers", "aldrin"])
    print (map' (+3) [1,5,3,1,6])
    print (filter' (>3) [1,5,3,2,1,6,4,3,2,1])
    print (largestDivisible)
    print (map2' (+1) [3,2,1,2,2])
    print (reverse2' [1,2,3,4,5])
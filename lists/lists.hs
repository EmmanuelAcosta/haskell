fibStep :: (Integer, Integer) -> (Integer, Integer)
fibStep (u, v) = (v, u + v)

fibPair :: Integer -> (Integer, Integer)
fibPair n
  | n == 0 = (0, 1)
  | otherwise = fibStep (fibPair (n -1))

fastFib :: Integer -> Integer
fastFib = fst . fibPair

{- [n .. m] is the list [n,n+1,...,m]; if n exceeds m, the list is empty.
[2 .. 7]  [2,3,4,5,6,7]
[3.1 .. 7.0]  [3.1,4.1,5.1,6.1,7.1]
[’a’ .. ’m’]  "abcdefghijklm" -}

--[ 2*n | n <- [2,4,7] ]
--[ 2*n | n <- [2,4,7], n > 3 ]
--[ 2*n | n <- [2,4,7], n > 3, n < 7 ]
--[ 2*n | n <- [2,4,7], n > 3, n < 7, n /= 5 ]
--[ 2*n | n <- [2,4,7], n > 3, n < 7, n /= 5, n == 4 ]


isEven :: Integer -> Bool
isEven n = (n `mod` 2 == 0)
--comprehension with predicate

list = [ 2*n | n <- [2,4,7], isEven n ]

addPairs :: [(Integer,Integer)] -> [Integer]
addPairs pairList = [ m+n | (m,n) <- pairList ]

allEven xs = xs == [x | x<-xs, isEven x]
allOdd xs = [] == [x | x<-xs, isEven x]

main = do
    print (isEven 4)
    print (isEven 5)
    print list
    print (addPairs [(1,2),(3,4),(5,6)])
    print (allEven [2,4,6])
    print (allEven [2,4,5])
    print (allOdd [1,3,5])
    print (allOdd [1,3,4])


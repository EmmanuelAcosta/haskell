-- create Rock, Paper and Scissors game

data Move = Rock | Paper | Scissors
  deriving (Show, Eq)

loose :: Move -> Move
loose Rock = Paper
loose Paper = Scissors
loose Scissors = Rock

win :: Move -> Move
win Rock = Scissors
win Paper = Rock
win Scissors = Paper

score :: Move -> Move -> Integer
score Rock Rock = 0
score Rock Paper = -1
score Rock Scissors = 1
score Paper Rock = 1
score Paper Paper = 0
score Paper Scissors = -1
score Scissors Rock = -1
score Scissors Paper = 1
score Scissors Scissors = 0

-- create a function that takes two moves and returns the winner

winner :: Move -> Move -> Move
winner a b
  | a == b = a
  | a == loose b = b
  | otherwise = a

-- create a function that takes two moves and returns the loser

loser :: Move -> Move -> Move
loser a b
  | a == b = a
  | a == loose b = a
  | otherwise = b

-- create function minAndMax that takes two moves and returns the minimum and maximum of the two moves

minAndMax :: Integer -> Integer -> (Integer, Integer)
minAndMax a b
  | a < b = (a, b)
  | otherwise = (b, a)

type ShopItem = (String, Int)

tupleShopItem :: ShopItem
tupleShopItem = ("Bread", 2)

name :: ShopItem -> String
price :: ShopItem -> Int
name (n, p) = n

price (n, p) = p

addPair :: (Integer, Integer) -> Integer
addPair p = fst p + snd p

fibStep :: (Integer, Integer) -> (Integer, Integer)
fibStep (u, v) = (v, u + v)

fibPair :: Integer -> (Integer, Integer)
fibPair n
  | n == 0 = (0, 1)
  | otherwise = fibStep (fibPair (n -1))

fastFib :: Integer -> Integer
fastFib = fst . fibPair

type Name = String

type Age = Int

data People = Person Name Age
  deriving (Eq, Show)

showPerson :: People -> String
showPerson (Person st n) = st ++ " -- " ++ show n

data Shape = Circle Float | Rectangle Float Float
  deriving (Eq, Show)

isRound :: Shape -> Bool
isRound (Circle _) = True
isRound (Rectangle _ _) = False

area :: Shape -> Float
area (Circle r) = pi * r ^ 2
area (Rectangle l w) = l * w

-- main function

main = do
  print $ winner Rock Paper
  print $ winner Rock Scissors
  print $ winner Paper Scissors
  print $ winner Paper Rock
  print $ winner Scissors Rock
  print $ winner Scissors Paper
  print $ winner Rock Rock
  print $ winner Paper Paper
  print $ winner Scissors Scissors

  print $ loser Rock Paper
  print $ loser Rock Scissors
  print $ loser Paper Scissors
  print $ loser Paper Rock
  print $ loser Scissors Rock
  print $ loser Scissors Paper
  print $ loser Rock Rock
  print $ loser Paper Paper
  print $ loser Scissors Scissors
  print $ minAndMax 1 2
  print $ name tupleShopItem
  print $ price tupleShopItem
  print $ addPair (1, 2)
  print $ fibPair 5
  print $ fastFib 5
  print $ score Rock Paper
  print $ score Rock Scissors
  print $ score Paper Scissors
  print $ score Paper Rock
  print $ score Scissors Rock
  print $ score Scissors Paper
  print $ score Rock Rock
  print $ score Paper Paper
  print $ score Scissors Scissors
  print $ showPerson (Person "John" 20)

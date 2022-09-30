exOr :: Bool -> Bool -> Bool
exOr x y = (x || y) && not (x && y)
exOr True x = not x
exOr False x = x

myNot :: Bool -> Bool
myNot True = False
myNot False = True

prop_myNot :: Bool -> Bool
prop_myNot x =
  not x == myNot x

{- prop_exOrs :: Bool -> Bool -> Bool
prop_exOrs x y =
    exOr x y == exOr1 x y -}

prop_exOr2 :: Bool -> Bool -> Bool
prop_exOr2 x y =
  exOr x y == (x /= y)

threeEqual :: Integer -> Integer -> Integer -> Bool
threeEqual m n p = (m == n) && (n == p)

max :: Integer -> Integer -> Integer
max x y
  | x >= y = x
  | otherwise = y

maxThree :: Integer -> Integer -> Integer -> Integer
maxThree x y z
  | x >= y && x >= z = x
  | y >= z = y
  | otherwise = z
{- fromEnum :: Char -> Int
toEnum :: Int -> Char -}

offset :: Int
offset = fromEnum 'A' - fromEnum 'a'
toUpper :: Char -> Char
toUpper ch = toEnum (fromEnum ch + offset)

isDigit :: Char -> Bool
isDigit ch = ('0' <= ch) && (ch <= '9')

isLower :: Char -> Bool
isLower ch = ('a' <= ch) && (ch <= 'z')

isUpper :: Char -> Bool
isUpper ch = ('A' <= ch) && (ch <= 'Z')

isAlpha :: Char -> Bool
isAlpha ch = isLower ch || isUpper ch

isAlphaNum :: Char -> Bool
isAlphaNum ch = isAlpha ch || isDigit ch

toLower :: Char -> Char
toLower ch = toEnum (fromEnum ch - offset)

{- (floor 5.6) + 6.7 -}

{- fromIntegral (floor 5.6) + 6.7 -}

{- sin (pi/4) * sqrt 2 -}

(&&&) :: Integer -> Integer -> Integer
x &&& y
    | x > y = y
    | otherwise = x

{- (+) :: Integer -> Integer -> Integer
(+) x y = x + y -}


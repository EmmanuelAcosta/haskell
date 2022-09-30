{- --data Bool = False | True
data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)

surface :: Shape -> Float
surface (Circle _ _ r) = pi * r ^ 2
surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)

main :: IO ()
main = do 
    print (surface (Circle 10 20 10))
    print (surface (Circle 11 24 10))
    print (surface (Circle 14 27 10))
    print (Circle 10 10 40) -}


{- data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))

baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height)

main :: IO ()
main = do
    print (surface (Rectangle (Point 0 0) (Point 100 100)))
    print (surface (Circle (Point 0 0) 24))
    print (nudge (Circle (Point 34 34) 10) 5 10)
    print (nudge (baseRect 40 100) 60 23) -}

{- 
data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Float
                     , phoneNumber :: String
                     , flavor :: String
                     } deriving (Show)

data Car = Car {company :: String, model :: String, year :: Int} deriving (Show)

--Car {company="Ford", model="Mustang", year=1967}

data Maybe a = Nothing | Just a

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
           deriving (Eq, Ord, Show, Read, Bounded, Enum)

--minBound :: Day
--[minBound .. maxBound] :: [Day]

phoneBook :: [(String,String)]
phoneBook =
    [("betty","555-2938")
    ,("bonnie","452-2928")
    ,("patsy","493-2928")
    ,("lucille","205-2928")
    ,("wendy","939-8282")
    ,("penny","853-2492")
    ]
type PhoneNumber = String
type Name = String
type PhoneBook = [(Name,PhoneNumber)]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name,pnumber) `elem` pbook

--data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)
data List a = Empty | Cons { listHead :: a, listTail :: List a} deriving (Show, Read, Eq, Ord) -}


--9. takePersonas: dada una lista de Personas [nombre, apellido y fecha de nacimiento]
-- (también declare un tipo de dato Date) ordenada ascendentemente por fecha de nacimiento; 
--y una fecha, devuelve el segmento más largo de la lista con las personas que nacieron antes 
--dicha fecha.

{- data Persona = Persona Nombre Apellido Fecha deriving Show
data Nombre = Nombre String deriving Show
data Apellido = Apellido String deriving Show
data Fecha =Fecha Int deriving Show

takePersonas :: [Persona] -> Fecha -> [Persona]
takePersonas [] _ = []
takePersonas (x:xs) n 
                | esMenor x n = x : (takePersonas xs n)
                | otherwise = []

takeAllPersonasMenores :: [Persona] -> Fecha -> [Persona]
takeAllPersonasMenores [] _ = []
takeAllPersonasMenores (x:xs) f 
                        | esMenor x f = x : takeAllPersonasMenores xs f
                        | otherwise = takeAllPersonasMenores xs f

esMenor :: Persona -> Fecha -> Bool
esMenor (Persona n a (Fecha fn)) (Fecha f) = fn < f

main :: IO ()
main = do
    print (takePersonas [(Persona (Nombre "Emmanuel1") (Apellido "Acosta1") (Fecha 6)),(Persona (Nombre "Emmanuel") (Apellido "Acosta") (Fecha 2)),(Persona (Nombre "Emmanuel2") (Apellido "Acosta2") (Fecha 11)),(Persona (Nombre "Emmanuel3") (Apellido "Acosta3") (Fecha 3))] (Fecha 10))
    print (takeAllPersonasMenores [(Persona (Nombre "Emmanuel1") (Apellido "Acosta1") (Fecha 6)),(Persona (Nombre "Emmanuel") (Apellido "Acosta") (Fecha 2)),(Persona (Nombre "Emmanuel2") (Apellido "Acosta2") (Fecha 11)),(Persona (Nombre "Emmanuel3") (Apellido "Acosta3") (Fecha 3)),(Persona (Nombre "Emmanuel5") (Apellido "Acosta5") (Fecha 5))] (Fecha 10))
 -}

 --10. dropPrecio: dada una lista de Pizzas [lista de ingredientes y precio] 
 --en orden ascendente por precio, devuelve el segmento más largo 
 --de la lista que comienza con la pizza que tiene el menor precio superior a $30

{- data Pizza = Pizza [Ingrediente] Precio deriving (Show)
data Ingrediente = Huevo | Queso | Harina | Levadura | Tomate | Jamon | Sal deriving Show
data Precio = Precio Int deriving (Show,Eq)

dropPrecio :: [Pizza] -> [Pizza]
dropPrecio [] = []
dropPrecio (x:xs) 
        | esPrecioMenor x 30 = x : dropPrecio xs
        | otherwise = []

esPrecioMenor :: Pizza -> Int -> Bool
esPrecioMenor (Pizza is (Precio pr)) n = pr < n -}

--11. takeNombresPersonas: dada una lista de Personas y una fecha devuelve los nombres de las personas 
--incluidas en segmento más largo de la lista con las personas que nacieron antes dicha fecha.

{- data Persona = Persona Nombre Apellido Fecha deriving Show
data Nombre = Nombre String deriving Show
data Apellido = Apellido String deriving Show
data Fecha =Fecha Int deriving Show


takeNombresPersonas :: [Persona] -> Fecha -> [Nombre]
takeNombresPersonas [] _ = []
takeNombresPersonas (x:xs) n 
                | esMenor x n = (getNombre x) : (takeNombresPersonas xs n)
                | otherwise = []

esMenor :: Persona -> Fecha -> Bool
esMenor (Persona n a (Fecha fn)) (Fecha f) = fn < f

getNombre :: Persona -> Nombre
getNombre (Persona n a f)=  n -}

--12. reversa: dada una lista de enteros, devuelve la lista con los mismos elementos de atrás para adelante.

{- reversa :: [Int] -> [Int]
reversa [] = []
reversa (x:xs) = reversa xs ++ [x] -}

--1. append: dadas dos listas devuelve la lista con todos los elementos de la primer 
-- lista y todos los elementos de la segunda a continuación.

{- append :: [a] -> [a] -> [a]
append [] [] = []
append (x:xs) (ys) = x : append xs ys
append [] (y:ys) = (y:ys) -}
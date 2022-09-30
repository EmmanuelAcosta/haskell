--1. mapSucesor: dada una lista de enteros, devuelve la lista de los sucesores de cada entero.
{- mapSuccesor :: [Int] -> [Int]
mapSuccesor [] = []
mapSuccesor (x:xs) = succ x : mapSuccesor xs

main :: IO ()
main = do
    print (mapSuccesor [1,2,3,4,5]) -}


--filterPositivos: dada una lista de enteros, devuelve una lista con los elementos que son positivos.

{- filterPositivos :: [Int] -> [Int]
filterPositivos [] = []
filterPositivos (x:xs) 
    | x > 0 = x : filterPositivos xs
    | otherwise = filterPositivos xs

main :: IO ()
main = do
    print (filterPositivos [1,2,-3]) -}

--zipMaximos: dadas dos listas de enteros, devuelve una lista 
--donde el elemento n es el máximo entre el elemento n de la lista 1 y de la lista 2.

{- zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos [] [] = []
zipMaximos (x:xs) (y:ys)  | x > y = x : zipMaximos xs ys
                          | otherwise = y : zipMaximos xs ys
zipMaximos (x:xs) ([]) = x:xs
zipMaximos ([]) (y:ys) = y:ys -}

 --zipSort: dadas dos listas de enteros de igual longitud,
 -- devuelve una lista de pares (min,max), donde min y 
 --max son el mínimo y el máximo entre los elementos de ambas listas en la misma posición.

{- zipSort :: [Int] -> [Int] -> [(Int,Int)]
zipSort [] [] = []
zipSort (x:xs) (y:ys)  | x > y = (y,x) : zipSort xs ys
                       | otherwise = (x,y) : zipSort xs ys
zipSort (x:xs) ([]) = []
zipSort ([]) (y:ys) = []

main :: IO ()
main = do 
    print (zipSort [3,1011,2,3,4,3,2] [90,123,3,2,23,4]) -}

--mapLongitudes: dada una lista de listas, devuelve la lista de sus longitudes.
{- mapLongitudes :: [[a]] -> [Int]
mapLongitudes [] = []
mapLongitudes (x:xs) = length x : mapLongitudes xs

main :: IO ()
main = do 
    print (mapLongitudes [[1,2,3,1,2,3,4],[2,3],[]]) -}


--longitudMayorA: dada una lista de listas y un número n,
-- devuelve la lista de aquellas listas que tienen más de n elementos.

{- longitudMayorA :: [[a]] -> Int -> [[a]]
longitudMayorA [] _ = []
longitudMayorA (x:xs) n 
                    | n < 0 = []
                    | length x >= n = (x : longitudMayorA xs n)
                    | otherwise = longitudMayorA xs n

main :: IO ()
main = do 
    print (longitudMayorA [[3,2],[1,2,3]] 2) -}

--7. mapCuadrados: dada una lista de enteros, 
--devuelve la lista de los cuadrados de los elementos positivos, en el mismo orden.

{- mapCuadrados :: [Int] -> [Int]
mapCuadrados [] = []
mapCuadrados (x:xs) 
        | x>0 = ((x^2): mapCuadrados xs)
        | otherwise = mapCuadrados xs
    
main :: IO ()
main = do
    print (mapCuadrados [1,2,3,-1,-4]) -}

-- sumaPar: dada una lista de pares, devuelve una nueva lista en 
--la que cada elemento es la suma de los elementos de cada par.

{- sumaPar :: [(Int,Int)] -> [Int]
sumaPar [] = []
sumaPar (x:xs) = (fst x + snd x) : sumaPar xs

main :: IO ()
main = do
    print (sumaPar [(2,3),(3,5)]) -}
    
capital :: String -> String
capital "" = "¡Una cadena vacía!"
capital all@(x:_) = "La primera letra de " ++ all ++ " es " ++ [x]

main :: IO ()
main = do
    print (capital "Dracula")
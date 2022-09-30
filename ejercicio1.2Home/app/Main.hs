module Main where

import Data.Map as Map

data JSON = JsonObj (Map.Map String JSON) | JsonBool Bool | JsonNum Double | JsonNull | JsonList [JSON] | JsonString String deriving (Eq, Show)

removeLast :: Char -> String -> String
removeLast c1 = snd . remLast
  where
    remLast :: String -> (Bool, String)
    remLast [] = (False, [])
    remLast (c2:cs) =
      case remLast cs of
        (True, cs') -> (True, c2:cs')
        (False, cs') -> if c1 == c2 then (True, cs') else (False, c2:cs')

stringify :: JSON -> String
stringify (JsonObj m) = removeLast ','  ("{" ++ (concatMap (\(k, v) -> "\"" ++ k ++ "\":" ++ (stringify v) ++ ",") (toList m)) ++ "}")
stringify (JsonBool b) = show b
stringify (JsonList l) = removeLast ',' ("[" ++ (concatMap (\v -> (stringify v) ++ ",") l) ++ "]")
stringify (JsonNum d) = show d
stringify (JsonString s) = "" ++ show s ++ ""
stringify JsonNull = "null"

main :: IO ()
main = do
    let jsonTree = JsonObj $ Map.fromList [("name", JsonString "John"), ("age", JsonNum 30), ("isMarried", JsonBool False), ("children", JsonObj $ fromList [("name", JsonString "Mar\'y"), ("age", JsonNum 5)]), ("spouse", JsonNull)]
    print $ jsonTree
    putStrLn $ stringify jsonTree

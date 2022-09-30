module Main where

-- import map
import Data.Map

data JSON = JsObj (Map String JSON) | JsBool Bool | JsList [JSON] | JsStr String | JsNull | JsNum Double deriving (Eq, Show)


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
stringify (JsObj m) = removeLast ',' ("{" ++ (concatMap (\(k, v) -> "\"" ++ k ++ "\":" ++ (stringify v) ++ ",") (toList m)) ++ "}")
stringify (JsBool b) = show b
stringify (JsList l) = removeLast ',' ("[" ++ (concatMap (\v -> (stringify v) ++ ",") l) ++ "]")
stringify (JsStr s) = "" ++ show s ++ ""
stringify JsNull = "null"
stringify (JsNum n) = show n


main :: IO ()
main = do
  --let jsonTree = JsonObject $ fromList [("name", JsonString "name" "John"), ("age", JsonNumber "age" 30), ("isMarried", JsonBool "isMarried" False), ("children", JsonObject $ fromList [("name", JsonString "name" "Mary"), ("age", JsonNumber "age" 5)]), ("spouse", JsonNull)]
  let jsonTree = JsObj $ fromList [("name", JsStr "John"), ("age", JsNum 30), ("isMarried", JsBool False), ("children", JsObj $ fromList [("name", JsStr "Mar\'y"), ("age", JsNum 5)]), ("spouse", JsNull)]
  let jsonExtraLarge = JsObj $ fromList [("name", JsStr "John"), ("age", JsNum 30), ("isMarried", JsBool False), ("children", JsList [JsObj $ fromList [("name", JsStr "Mar\ny"), ("age", JsNum 5)], JsObj $ fromList [("name", JsStr "John"), ("age", JsNum 3)]])]
  let jsonExtraLargeWithLists = JsObj $ fromList [("name", JsStr "John"), ("age", JsNum 30), ("isMarried", JsBool False), ("children", JsList [JsObj $ fromList [("name", JsStr "Mary"), ("age", JsNum 5)], JsObj $ fromList [("name", JsStr "John"), ("age", JsNum 3)]])]
  let jsonEmptyObject = JsObj $ fromList []
  putStrLn $ stringify jsonEmptyObject
  putStrLn $ stringify jsonExtraLargeWithLists
  putStrLn $ stringify jsonExtraLarge
  putStrLn $ stringify jsonTree

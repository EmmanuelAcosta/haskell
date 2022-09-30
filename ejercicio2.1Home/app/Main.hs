import Data.Char (isDigit, isSpace)
import Data.List (intercalate)
import Data.Map.Strict (Map, fromList, (!))
import Text.Read (readMaybe)

type SchemyEnv = Map String SchemyExp
type Procedure = SchemyEnv -> [SchemyExp] -> SchemyExp



data SchemyExp = SchemyNumber Double
    | SchemyAdd SchemyExp SchemyExp
    | SchemyMult SchemyExp SchemyExp
    | SchemySymbol String
    | SchemyBool Bool
    | SchemyForm SchemyExp [SchemyExp]
    | SchemyProcedure Procedure

instance Show SchemyExp where
  show (SchemyBool b) = "(SchemyBool "++ (show b) ++")"
  show (SchemyNumber d) = "(SchemyNumber "++ (show d) ++")"
  show (SchemySymbol s) = "(SchemySymbol "++ (show s) ++")"
  show (SchemyForm f args) = "(SchemyForm "++ (show f) ++" "++ (show args) ++")"
  show (SchemyProcedure p) = "(SchemyProcedure ?)"
  show (SchemyAdd x y) = "(SchemyAdd "++ (show x) ++" "++ (show y) ++")"
  show (SchemyMult x y) = "(SchemyMult "++ (show x) ++" "++ (show y) ++")"

eval :: (Map String SchemyExp) -> SchemyExp -> SchemyExp
eval env (SchemyNumber x) = SchemyNumber x
eval env (SchemyAdd x y) = SchemyNumber ((evalDouble env x) + (evalDouble env y))
eval env (SchemyMult x y) = SchemyNumber ((evalDouble env x) * (evalDouble env y))
eval env (SchemySymbol x) = env ! x
eval env (SchemyBool x) = SchemyBool x
eval env (SchemyProcedure p) = SchemyProcedure p
eval env (SchemyForm (SchemyProcedure p) args) = p env args
eval env (SchemyForm (SchemySymbol s) args) = eval env (SchemyForm (env ! s) args)
eval env (SchemyForm f args) = eval env (SchemyForm (eval env f) args)







evalDouble :: SchemyEnv -> SchemyExp -> Double
evalDouble env exp = n
  where (SchemyNumber n) = eval env exp
evalDouble env _ = error "error"

evalBool :: SchemyEnv -> SchemyExp -> Bool
evalBool env exp = n
    where (SchemyBool n) = eval env exp
evalBool env _ = error "error"

evalProcedure env exp = case eval env exp of
  SchemyProcedure p -> p
  _ -> error ("Expected procedure for "++ (show exp) ++"!")

aor :: Procedure -> Procedure
aor p env args = p env (map (eval env) args) 

procAdd :: Procedure
procAdd = aor (\_ [SchemyNumber n1, SchemyNumber n2] -> SchemyNumber (n1 + n2))

procEq :: Procedure
procEq = aor (\_ [SchemyNumber n1, SchemyNumber n2] -> SchemyBool (n1 == n2))

procAnd :: Procedure
procAnd = aor (\_ [SchemyBool b1, SchemyBool b2] -> SchemyBool (b1 && b2))

unparse :: SchemyExp -> String
unparse (SchemyBool b) = if b then "true" else "false"
unparse (SchemyNumber d) = show d
unparse (SchemySymbol s) = s
unparse (SchemyForm f args) = "("++ (intercalate " " (map unparse (f:args))) ++")"
unparse (SchemyProcedure _) = error "Cannot unparse procedures!"

mayParse :: String -> Maybe (SchemyExp, String)
mayParse input
  | input == "" = Nothing
  | isSpace (head input) = mayParse (tail input)
  | (head input) == '(' = mayParseForm [] (tail input)
  | otherwise = if (token == "true") then Just (SchemyBool True, rest)
    else if (token == "false") then Just (SchemyBool False, rest)
    else case readMaybe token of
      Just n -> Just (SchemyNumber n, rest)
      _ -> Just (SchemySymbol token, rest)
  where token = takeWhile (\s -> notElem s " ()\f\n\r\t") input
        rest = drop (length token) input

mayParseForm :: [SchemyExp] -> String -> Maybe (SchemyExp, String)
mayParseForm list input
  | input == "" = Nothing
  | isSpace (head input) = mayParseForm list (tail input)
  | (head input) == ')'  = Just (SchemyForm (head list) (tail list), (tail input))
  | otherwise = case mayParse input of
    Just (exp, rest) -> mayParseForm (list ++ [exp]) rest
    _ -> Nothing

parse :: String -> SchemyExp
parse input = case mayParse input of
  Just (exp, rest) | all isSpace rest -> exp
  _ -> error "Parse error!"

maxrec :: (Ord a) => [a] -> a
maxrec [] = error "maximum of empty list"
maxrec [x] = x
maxrec (x:xs)
  | x > maxTail = x
  | otherwise = maxTail
  where maxTail = maxrec xs

basicEnv :: SchemyEnv
basicEnv = fromList [("+", SchemyProcedure (\env [x, y] -> SchemyNumber ((evalDouble env x) + (evalDouble env y)))),
                           ("-", SchemyProcedure (\env [x, y] -> SchemyNumber ((evalDouble env x) - (evalDouble env y)))),
                           ("pi", SchemyNumber 3.1416),
                           ("*", SchemyProcedure (\env [x, y] -> SchemyNumber ((evalDouble env x) * (evalDouble env y)))),
                           ("/", SchemyProcedure (\env [x, y] -> SchemyNumber ((evalDouble env x) / (evalDouble env y)))),
                           (">", SchemyProcedure (\env [x, y] -> SchemyBool ((evalDouble env x) > (evalDouble env y)))),
                           ("<", SchemyProcedure (\env [x, y] -> SchemyBool ((evalDouble env x) < (evalDouble env y)))),
                           (">=", SchemyProcedure (\env [x, y] -> SchemyBool ((evalDouble env x) >= (evalDouble env y)))),
                           ("<=", SchemyProcedure (\env [x, y] -> SchemyBool ((evalDouble env x) <= (evalDouble env y)))),
                           ("==", SchemyProcedure (\env [x, y] -> SchemyBool ((evalDouble env x) == (evalDouble env y)))),
                           ("&&", SchemyProcedure (\env [x, y] -> SchemyBool ((evalBool env x) && (evalBool env y)))),
                           ("||", SchemyProcedure (\env [x, y] -> SchemyBool ((evalBool env x) || (evalBool env y)))),
                           ("!", SchemyProcedure (\env [x] -> SchemyBool (not (evalBool env x))))]
-- function to convert fromList [("max", maxProcedure)] to SchemyEnv

repl :: IO ()
repl = do
  line <- getLine
  if not (all isSpace line) then do
    --putStrLn (show (parse line)) -- Show the parse result
    putStrLn (unparse (parse line)) -- Echo the code
    --putStrLn (unparse (eval basicEnv (parse line))) -- Print the evaluation
    repl
  else
    return ()

main :: IO ()
main = do
  let env = fromList [("x", (SchemyNumber 1.0)), ("y", (SchemyNumber 2.0))]
  print $ "Adding 1 and 2:"
  print $ eval env (SchemyAdd (SchemyNumber 1.0) (SchemyNumber 2.0))
  print $ "Mult 2 and 2:"
  print $ eval env (SchemyMult (SchemyNumber 2.0) (SchemyNumber 2.0))
  print $ "Symbol x:"
  print $ eval env (SchemySymbol "x")
  print $ "Boolean"
  print $ eval env (SchemyBool True)
  print $ "Procedure:"
  print $ eval env (SchemyProcedure (\_ _ -> SchemyNumber 1.0))
  print $ "Procedure:"
  print (SchemyProcedure (\_ _ -> SchemyNumber 1.0))
  print $ "Procedure:"
  print $ eval env (SchemyForm (SchemyProcedure (\_ _ -> SchemyNumber 1.0)) [])
  let maxProcedure = SchemyProcedure (\_ list -> SchemyNumber (maxrec (map (evalDouble env) list)))
  let env2 = fromList [("max", maxProcedure)]
  print $ "Max of list:"
  print $ eval env2 (SchemyForm (SchemySymbol "max") [SchemyNumber 1.0, SchemyNumber 2.0,SchemyNumber 6.0])
  --basicEnv has operations like +, -, *, /, >, <, >=, <=, =, and, or, not
  
  --Description of adding
  print $ "Adding 1 and 2:"
  print $ eval basicEnv (SchemyForm (SchemySymbol "+") [SchemyNumber 1.0, SchemyNumber 2.0])
  print $ "Subtracting 7 and 2:"
  print $ eval basicEnv (SchemyForm (SchemySymbol "-") [SchemyNumber 7.0, SchemyNumber 2.0])
  print $ "Multiplying 4 and 2:"
  print $ eval basicEnv (SchemyForm (SchemySymbol "*") [SchemyNumber 4.0, SchemyNumber 2.0])
  print $ "Dividing 4 and 2:"
  print $ eval basicEnv (SchemyForm (SchemySymbol "/") [SchemyNumber 4.0, SchemyNumber 2.0])
  print $ "1 > 2:"
  print $ eval basicEnv (SchemyForm (SchemySymbol ">") [SchemyNumber 1.0, SchemyNumber 2.0])
  print $ "1 < 2:"
  print $ eval basicEnv (SchemyForm (SchemySymbol "<") [SchemyNumber 1.0, SchemyNumber 2.0])
  print $ "1 >= 2:"
  print $ eval basicEnv (SchemyForm (SchemySymbol ">=") [SchemyNumber 1.0, SchemyNumber 2.0])
  print $ "1 <= 2:"
  print $ eval basicEnv (SchemyForm (SchemySymbol "<=") [SchemyNumber 1.0, SchemyNumber 2.0])
  print $ "1 = 2:"
  print $ eval basicEnv (SchemyForm (SchemySymbol "==") [SchemyNumber 1.0, SchemyNumber 2.0])
  print $ "True and False:"
  print $ eval basicEnv (SchemyForm (SchemySymbol "&&") [SchemyBool True, SchemyBool False])
  print $ "True or False:"
  print $ eval basicEnv (SchemyForm (SchemySymbol "||") [SchemyBool True, SchemyBool False])
  print $ "not True:"
  print $ eval basicEnv (SchemyForm (SchemySymbol "!") [SchemyBool True])
  --SchemyForm who add two numbers and then multiply them
  print $ eval basicEnv (SchemyForm (SchemySymbol "+") [SchemyNumber 2.0, SchemyNumber 2.0])
  print $ eval basicEnv (SchemySymbol "+")
  print $ eval basicEnv (SchemyForm (SchemySymbol "*") [(SchemyForm (SchemySymbol "+")[SchemyNumber 2.0, SchemyNumber 2.0]), SchemyNumber 2.0])
  repl
  




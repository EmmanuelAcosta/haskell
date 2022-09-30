import Data.Map.Strict (fromList, Map, (!))
import Data.Either

type SchemyEnv = Map String SchemyExp
type Procedure = SchemyEnv -> [SchemyExp] -> SchemyExp



data SchemyExp = SchemyNumber Double
    | SchemyAdd SchemyExp SchemyExp
    | SchemyMult SchemyExp SchemyExp
    | SchemySymbol String
    | SchemyBool Bool
    | SchemyForm SchemyExp [SchemyExp]
    | SchemyEnv SchemyEnv
    | SchemyProcedure Procedure

instance Show SchemyExp where
  show (SchemyBool b) = "(SchemyBool "++ (show b) ++")"
  show (SchemyNumber d) = "(SchemyNumber "++ (show d) ++")"
  show (SchemySymbol s) = "(SchemySymbol "++ (show s) ++")"
  show (SchemyForm f args) = "(SchemyForm "++ (show f) ++" "++ (show args) ++")"
  show (SchemyProcedure p) = "(SchemyProcedure ?)"
  show (SchemyEnv e) = "(SchemyEnv ?)"
  show (SchemyAdd x y) = "(SchemyAdd "++ (show x) ++" "++ (show y) ++")"
  show (SchemyMult x y) = "(SchemyMult "++ (show x) ++" "++ (show y) ++")"

eval :: (Map String SchemyExp) -> SchemyExp -> SchemyExp
eval env (SchemyNumber x) = SchemyNumber x
eval env (SchemyAdd x y) = SchemyNumber ((evalDouble env x) + (evalDouble env y))
eval env (SchemyMult x y) = SchemyNumber ((evalDouble env x) * (evalDouble env y))
eval env (SchemySymbol x) = env ! x
eval env (SchemyBool x) = SchemyBool x
eval env (SchemyProcedure p) = p env []
eval env (SchemyForm (SchemyProcedure p) args) = p env args
eval env (SchemyForm (SchemySymbol s) args) = eval env (SchemyForm (env ! s) args)
eval env (SchemyForm f args) = error "error"





evalDouble :: SchemyEnv -> SchemyExp -> Double
evalDouble env exp = n
  where (SchemyNumber n) = eval env exp
evalDouble env _ = error "error"

evalBool :: SchemyEnv -> SchemyExp -> Bool
evalBool env exp = n
    where (SchemyBool n) = eval env exp 
evalBool env _ = error "error"



-- function to convert fromList [("max", maxProcedure)] to SchemyEnv


main :: IO ()
main = do
  let env = fromList [("x", (SchemyNumber 1.0)), ("y", (SchemyNumber 2.0))]
  print $ "Adding"
  print $ eval env (SchemyAdd (SchemyNumber 1.0) (SchemyNumber 2.0))
  print $ "Mult"
  print $ eval env (SchemyMult (SchemyNumber 2.0) (SchemyNumber 2.0))
  print $ "Symbol"
  print $ eval env (SchemySymbol "x")
  print $ "Boolean"
  print $ eval env (SchemyBool True)
  print $ "Procedure:"
  print $ eval env (SchemyProcedure (\_ _ -> SchemyNumber 1.0))
  print $ "Procedure:"
  print (SchemyProcedure (\_ _ -> SchemyNumber 1.0))
  print $ "Procedure:"
  print $ eval env (SchemyForm (SchemyProcedure (\_ _ -> SchemyNumber 1.0)) [])
  let maxProcedure = SchemyProcedure (\_ [x, y] -> if (evalDouble env x) > (evalDouble env y) then x else y)
  let env2 = fromList [("max", maxProcedure)]
  print $ "Max of 1 and 2:"
  print $ eval env2 (SchemyForm (SchemySymbol "max") [SchemyNumber 1.0, SchemyNumber 2.0])
  --basicEnv has operations like +, -, *, /, >, <, >=, <=, =, and, or, not
  let basicEnv = fromList [("+", SchemyProcedure (\_ [x, y] -> SchemyNumber ((evalDouble env x) + (evalDouble env y)))),
                           ("-", SchemyProcedure (\_ [x, y] -> SchemyNumber ((evalDouble env x) - (evalDouble env y)))),
                           ("*", SchemyProcedure (\_ [x, y] -> SchemyNumber ((evalDouble env x) * (evalDouble env y)))),
                           ("/", SchemyProcedure (\_ [x, y] -> SchemyNumber ((evalDouble env x) / (evalDouble env y)))),
                           (">", SchemyProcedure (\_ [x, y] -> SchemyBool ((evalDouble env x) > (evalDouble env y)))),
                           ("<", SchemyProcedure (\_ [x, y] -> SchemyBool ((evalDouble env x) < (evalDouble env y)))),
                           (">=", SchemyProcedure (\_ [x, y] -> SchemyBool ((evalDouble env x) >= (evalDouble env y)))),
                           ("<=", SchemyProcedure (\_ [x, y] -> SchemyBool ((evalDouble env x) <= (evalDouble env y)))),
                           ("=", SchemyProcedure (\_ [x, y] -> SchemyBool ((evalDouble env x) == (evalDouble env y)))),
                           ("and", SchemyProcedure (\_ [x, y] -> SchemyBool ((evalBool env x) && (evalBool env y)))),
                           ("or", SchemyProcedure (\_ [x, y] -> SchemyBool ((evalBool env x) || (evalBool env y)))),
                           ("not", SchemyProcedure (\_ [x] -> SchemyBool (not (evalBool env x))))]
  --Description of adding
  print $ "Adding 1 and 2:"
  print $ eval basicEnv (SchemyForm (SchemySymbol "+") [SchemyNumber 1.0, SchemyNumber 2.0])
  print $ "Subtracting 1 and 2:"
  print $ eval basicEnv (SchemyForm (SchemySymbol "-") [SchemyNumber 1.0, SchemyNumber 2.0])
  print $ "Multiplying 1 and 2:"
  print $ eval basicEnv (SchemyForm (SchemySymbol "*") [SchemyNumber 1.0, SchemyNumber 2.0])
  print $ "Dividing 1 and 2:"
  print $ eval basicEnv (SchemyForm (SchemySymbol "/") [SchemyNumber 1.0, SchemyNumber 2.0])
  print $ "1 > 2:"
  print $ eval basicEnv (SchemyForm (SchemySymbol ">") [SchemyNumber 1.0, SchemyNumber 2.0])
  print $ "1 < 2:"
  print $ eval basicEnv (SchemyForm (SchemySymbol "<") [SchemyNumber 1.0, SchemyNumber 2.0])
  print $ "1 >= 2:"
  print $ eval basicEnv (SchemyForm (SchemySymbol ">=") [SchemyNumber 1.0, SchemyNumber 2.0])
  print $ "1 <= 2:"
  print $ eval basicEnv (SchemyForm (SchemySymbol "<=") [SchemyNumber 1.0, SchemyNumber 2.0])
  print $ "1 = 2:"
  print $ eval basicEnv (SchemyForm (SchemySymbol "=") [SchemyNumber 1.0, SchemyNumber 2.0])
  print $ "True and False:"
  print $ eval basicEnv (SchemyForm (SchemySymbol "and") [SchemyBool True, SchemyBool False])
  print $ "True or False:"
  print $ eval basicEnv (SchemyForm (SchemySymbol "or") [SchemyBool True, SchemyBool False])
  print $ "not True:"
  print $ eval basicEnv (SchemyForm (SchemySymbol "not") [SchemyBool True])


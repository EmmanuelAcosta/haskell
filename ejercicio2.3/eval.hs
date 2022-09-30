import qualified Data.Map as Map
import Data.Map.Strict (Map, fromList)
import Distribution.SPDX (LicenseId (AFL_3_0))

instance Show SchemyExp where
  show (SchemyBool b) = "(SchemyBool " ++ (show b) ++ ")"
  show (SchemyNumber d) = "(SchemyNumber " ++ (show d) ++ ")"
  show (SchemySymbol s) = "(SchemySymbol " ++ (show s) ++ ")"
  show (SchemyForm exps) = "(SchemyForm " ++ (show exps) ++ ")"
  show (SchemyProcedure p) = "(SchemyProcedure ?)"

data SchemyExp
  = SchemyBool Bool
  | SchemyNumber Double
  | SchemySymbol String
  | SchemyProcedure ([SchemyExp] -> SchemyExp)
  | SchemyForm [SchemyExp]
  deriving (Show)

type SchemyEnv = Map String SchemyExp

type Procedure = SchemyEnv -> [SchemyExp] -> SchemyExp

eval :: SchemyEnv -> SchemyExp -> SchemyExp
eval env (SchemySymbol s) = case Map.lookup s env of
  Just v -> v
  Nothing -> error ("Unbound variable: " ++ s)
eval env (SchemyProcedure p) = SchemyProcedure p
eval env (SchemyBool b) = SchemyBool b
eval env (SchemyNumber d) = SchemyNumber d
eval env (SchemyForm (SchemySymbol "quote" : exp : [])) = exp
eval env (SchemyForm (SchemySymbol "if" : pred : conseq : alt : [])) =
  case eval env pred of
    SchemyBool True -> eval env conseq
    SchemyBool False -> eval env alt
    _ -> error "if predicate must be a boolean"
eval env (SchemyForm (SchemySymbol "define" : SchemySymbol var : exp : [])) =
  let val = eval env exp
   in Map.insert var val env
eval env (SchemyForm (SchemySymbol "set!" : SchemySymbol var : exp : [])) =
  let val = eval env exp
   in case Map.lookup var env of
        Just _ -> Map.insert var val env
        Nothing -> error ("Unbound variable: " ++ var)
eval env (SchemyForm (SchemySymbol "lambda" : SchemyForm params : body : [])) =
  SchemyProcedure (\args -> eval (extendEnv params args env) body)
eval env (SchemyForm (SchemySymbol "begin" : exps)) =
  case exps of
    [] -> SchemyBool True
    [exp] -> eval env exp
    (exp : exps) -> eval env (SchemyForm exps)
eval env (SchemyForm (SchemySymbol "let" : SchemyForm bindings : body : [])) =
  let (vars, vals) = unzip (map unwrapBinding bindings)
      env' = extendEnv (map SchemySymbol vars) (map (eval env) vals) env
   in eval env' body
  where
    unwrapBinding (SchemyForm (SchemySymbol var : val : [])) = (var, val)
    unwrapBinding _ = error "let bindings must be a list of (var val)"
eval env (SchemyForm (SchemySymbol "letrec" : SchemyForm bindings : body : [])) =
  let (vars, vals) = unzip (map unwrapBinding bindings)
      env' = extendEnv (map SchemySymbol vars) (repeat (SchemyBool False)) env
      env'' = extendEnv (map SchemySymbol vars) (map (eval env') vals) env'
   in eval env'' body
  where
    unwrapBinding (SchemyForm (SchemySymbol var : val : [])) = (var, val)
    unwrapBinding _ = error "letrec bindings must be a list of (var val)"
eval env (SchemyForm (SchemySymbol "let*" : SchemyForm bindings : body : [])) =
  let (vars, vals) = unzip (map unwrapBinding bindings)
      env' = extendEnv (map SchemySymbol vars) (map (eval env) vals) env
   in eval env' body
  where
    unwrapBinding (SchemyForm (SchemySymbol var : val : [])) = (var, val)
    unwrapBinding _ = error "let* bindings must be a list of (var val)"
eval env (SchemyForm (SchemySymbol "cond" : clauses)) =
  case clauses of
    [] -> SchemyBool False
    (SchemyForm (SchemySymbol "else" : exp : []) : _) -> eval env exp
    (SchemyForm (pred : exp : []) : rest) ->
      case eval env pred of
        SchemyBool True -> eval env exp
        SchemyBool False -> eval env (SchemyForm (SchemySymbol "cond" : rest))
        _ -> error "cond predicate must be a boolean"
    _ -> error "cond clauses must be a list of (pred exp)"
eval env (SchemyForm (SchemySymbol "and" : exps)) =
  case exps of
    [] -> SchemyBool True
    [exp] -> eval env exp
    (exp : exps) ->
      case eval env exp of
        SchemyBool True -> eval env (SchemyForm (SchemySymbol "and" : exps))
        SchemyBool False -> SchemyBool False
        _ -> error "and operands must be booleans"
eval env (SchemyForm (SchemySymbol "or" : exps)) =
  case exps of
    [] -> SchemyBool False
    [exp] -> eval env exp
    (exp : exps) ->
      case eval env exp of
        SchemyBool True -> SchemyBool True
        SchemyBool False -> eval env (SchemyForm (SchemySymbol "or" : exps))
        _ -> error "or operands must be booleans"
eval env (SchemyForm (SchemySymbol "not" : exp : [])) =
  case eval env exp of
    SchemyBool b -> SchemyBool (not b)
    _ -> error "not operand must be a boolean"
eval env (SchemyForm (SchemySymbol "eq?" : exp1 : exp2 : [])) =
  case (eval env exp1, eval env exp2) of
    (SchemyBool b1, SchemyBool b2) -> SchemyBool (b1 == b2)
    (SchemyNumber d1, SchemyNumber d2) -> SchemyBool (d1 == d2)
    (SchemySymbol s1, SchemySymbol s2) -> SchemyBool (s1 == s2)
    (SchemyForm exps1, SchemyForm exps2) -> SchemyBool (exps1 == exps2)
    _ -> error "eq? operands must be of the same type"
eval env (SchemyForm (SchemySymbol "eqv?" : exp1 : exp2 : [])) =
  case (eval env exp1, eval env exp2) of
    (SchemyBool b1, SchemyBool b2) -> SchemyBool (b1 == b2)
    (SchemyNumber d1, SchemyNumber d2) -> SchemyBool (d1 == d2)
    (SchemySymbol s1, SchemySymbol s2) -> SchemyBool (s1 == s2)
    (SchemyForm exps1, SchemyForm exps2) -> SchemyBool (exps1 == exps2)
    _ -> error "eqv? operands must be of the same type"
eval env (SchemyForm (SchemySymbol "equal?" : exp1 : exp2 : [])) =
  case (eval env exp1, eval env exp2) of
    (SchemyBool b1, SchemyBool b2) -> SchemyBool (b1 == b2)
    (SchemyNumber d1, SchemyNumber d2) -> SchemyBool (d1 == d2)
    (SchemySymbol s1, SchemySymbol s2) -> SchemyBool (s1 == s2)
    (SchemyForm exps1, SchemyForm exps2) -> SchemyBool (exps1 == exps2)
    _ -> error "equal? operands must be of the same type"
eval env (SchemyForm (SchemySymbol "if" : pred : conseq : alt : [])) =
  case eval env pred of
    SchemyBool True -> eval env conseq
    SchemyBool False -> eval env alt
    _ -> error "if predicate must be a boolean"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      contents <- readFile filename
      let exps = readSchemy contents
      case exps of
        Left err -> print err
        Right exps' -> print (eval emptyEnv (SchemyForm exps'))
    _ -> putStrLn "Usage: schemy <filename>"
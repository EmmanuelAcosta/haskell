import Data.Map.Strict (fromList, Map, (!))
import Data.Either

type SchemyEnv = Map String Double
type Procedure = SchemyEnv -> [SchemyExp] -> SchemyExp

data SchemyExp = SchemyNumber Double
    | SchemyAdd SchemyExp SchemyExp
    | SchemyMult SchemyExp SchemyExp
    | SchemySymbol String
    | SchemyBool Bool
    | SchemyProcedure Procedure
    | SchemyForm SchemyExp [SchemyExp]
    deriving (Eq, Show)

eval :: SchemyEnv -> SchemyExp -> SchemyExp
eval env (SchemyNumber x) = SchemyNumber x
eval env (SchemyAdd x y) = SchemyNumber ((evalDouble env x) + (evalDouble env y))
eval env (SchemyMult x y) = SchemyNumber ((evalDouble env x) * (evalDouble env y))
eval env (SchemySymbol x) = SchemyNumber (env ! x)
eval env (SchemyBool x) = SchemyBool x
eval env (SchemyForm sym list) = (evalSymbol env sym)

evalDouble :: SchemyEnv -> SchemyExp -> Double
evalDouble env exp = n
  where (SchemyNumber n) = eval env exp
evalDouble env _ = error "error"

evalBool :: SchemyEnv -> SchemyExp -> Bool
evalBool env exp = n
    where (SchemyBool n) = eval env exp 
evalBool env _ = error "error"

evalSymbol :: SchemyEnv -> SchemyExp -> Double
evalSymbol env exp = n
    where (SchemySymbol n) = eval env exp 
evalSymbol env _ = error "error"

toSchemyNumber :: Double -> SchemyExp
toSchemyNumber a = SchemyNumber a

toSchemyBool :: Bool -> SchemyExp
toSchemyBool a = SchemyBool a


-- convert fromList SchemyProcedure to SchemyExp

main :: IO ()
main = do
    -- let env = fromList [("pi", 3.14), ("e", exp 1.0)]
    print 1
  
  
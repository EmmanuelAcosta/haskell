import Distribution.SPDX (LicenseId(AFL_3_0))
import Data.Map.Strict (Map,fromList)
import qualified Data.Map as Map

data SchemyExp = SchemyNumber Double
  | SchemyAdd SchemyExp SchemyExp
  | SchemyMult SchemyExp SchemyExp
  | SchemySymbol String
  deriving (Eq, Show)

eval :: SchemyExp -> Map.Map String Double -> Double
eval (SchemyNumber n) _ = n
eval (SchemyAdd e1 e2) env = (eval e1 env) + (eval e2 env)
eval (SchemyMult e1 e2) env = (eval e1 env) * (eval e2 env)
eval (SchemySymbol s) env = case Map.lookup s env of
    Just n -> n
    Nothing -> error "Symbol not found"


main :: IO ()
main = do
    let env = fromList [("pi", 3.14), ("e", exp 1.0)]
    print $ eval (SchemyAdd (SchemyNumber 1) (SchemyNumber 2)) Map.empty
    print $ eval (SchemySymbol "pi") env
    print $ eval (SchemySymbol "e") env
    print $ eval (SchemySymbol "nohacenada") env


    
    
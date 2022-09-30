import Distribution.SPDX (LicenseId(AFL_3_0))
import Data.Map.Strict (Map,fromList)
import qualified Data.Map as Map

data SchemyExp = SchemyNumber Double
  | SchemyAdd SchemyExp SchemyExp
  | SchemyMult SchemyExp SchemyExp
  | SchemySymbol String
  | SchemyBool Bool
  deriving (Show, Eq)

eval :: Map String SchemyExp -> SchemyExp -> SchemyExp
eval _ (SchemyNumber n) = SchemyNumber n
eval _ (SchemyBool b) = SchemyBool b
eval env (SchemyAdd e1 e2) = SchemyNumber (n1 + n2)
  where
    SchemyNumber n1 = eval env e1
    SchemyNumber n2 = eval env e2
eval env (SchemyMult e1 e2) = SchemyNumber (n1 * n2)
  where
    SchemyNumber n1 = eval env e1
    SchemyNumber n2 = eval env e2
eval env (SchemySymbol s) = case Map.lookup s env of
  Just e -> eval env e
  Nothing -> error $ "Symbol not found: " ++ s


type SchemyEnv = Map String SchemyExp



main :: IO ()
main = do
  let env = fromList [("x", SchemyNumber 1), ("y", SchemyNumber 2)]
  print $ eval env (SchemyAdd (SchemySymbol "x") (SchemySymbol "y"))
  print $ eval env (SchemyMult (SchemySymbol "x") (SchemySymbol "y"))
  print $ eval env (SchemyAdd (SchemySymbol "x") (SchemySymbol "z"))
  --bool
  let env2 = fromList [("x", SchemyBool True), ("y", SchemyBool False)]
  print $ eval env2 (SchemyAdd (SchemySymbol "x") (SchemySymbol "y"))
  print $ eval env2 (SchemyMult (SchemySymbol "x") (SchemySymbol "y"))
  print $ eval env2 (SchemyAdd (SchemySymbol "x") (SchemySymbol "z"))


  




    
    
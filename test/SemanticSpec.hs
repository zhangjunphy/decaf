{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module SemanticSpec where

import qualified AST
import Data.ByteString.Lazy (ByteString)
import qualified Parser
import qualified Semantic
import qualified SourceLoc as SL
import Test.Hspec
import Control.Lens (view)
import Data.Generics.Labels ()

spec :: Spec
spec = do
  describe "semantic analysis" $ do
    typeCheckSpec

program :: ByteString
program =
  "import a; \
  \ import print; \
  \ int a[3]; \
  \ bool b; \
  \ int c; \
  \ void main() { \
  \    a = b ? 1 : 3; \
  \    print('\\\\'); \
  \    print(\"hello\\\" world\"); \
  \}"

typeCheckSpec :: SpecWith ()
typeCheckSpec = do
  it "type check" $
    let (Right p) = Parser.parse program
        (Right (AST.ASTRoot imports vars methods, _, st)) = Semantic.runSemanticAnalysis p
        stmts = view #stmts $ view #block (SL.unLocate $ head methods)
        (AST.AssignStmt (AST.Assignment _ _ (Just expr))) = SL.unLocate $ head stmts
        tpe = view #tpe $ SL.unLocate expr
     in tpe == AST.IntType

{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module SemanticSpec where

import qualified AST
import Control.Lens (view)
import Data.ByteString.Lazy (ByteString)
import Data.Generics.Labels ()
import qualified Parser
import qualified Semantic
import qualified SourceLoc as SL
import Test.Hspec
import Util

spec :: Spec
spec = do
  describe "semantic analysis" $ do
    typeCheckSpec

program :: String
program = "test/samples/type_check.dcf"

typeCheckSpec :: SpecWith ()
typeCheckSpec = do
  it "type check" $
    checkSample program pred
  where
    typeOfAssignExpr stmt  = 
      let (AST.AssignStmt (AST.Assignment _ _ (Just expr))) = SL.unLocate stmt
      in view #tpe $ SL.unLocate expr
    pred program =
      let (Right p) = Parser.parse program
          (Right (AST.ASTRoot imports vars methods, _, st)) = Semantic.runSemanticAnalysis p
          stmts = view #stmts $ view #block (SL.unLocate $ head methods)
          (AST.AssignStmt (AST.Assignment _ _ (Just expr))) = SL.unLocate $ head stmts
          tpe = view #tpe $ SL.unLocate expr
       in typeOfAssignExpr (stmts!!0) == AST.IntType &&
          typeOfAssignExpr (stmts!!1) == AST.BoolType

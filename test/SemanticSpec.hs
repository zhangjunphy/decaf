-- Copyright (C) 2018-2024 Jun Zhang <zhangjunphy[at]gmail[dot]com>
--
-- This file is a part of decafc.
--
-- decafc is free software: you can redistribute it and/or modify it under the
-- terms of the MIT (X11) License as described in the LICENSE file.
--
-- decafc is distributed in the hope that it will be useful, but WITHOUT ANY
-- WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE.  See the X11 license for more details.

module SemanticSpec where

import qualified AST
import Control.Lens (view)
import Data.ByteString.Lazy (ByteString)
import Data.Generics.Labels ()
import qualified Parser
import qualified Semantic
import qualified Util.SourceLoc as SL
import Test.Hspec
import Util

spec :: Spec
spec = do
  describe "semantic analysis" $ do
    typeCheckSpec

program :: String
program = "test/samples/semantic/type_check.dcf"

typeCheckSpec :: SpecWith ()
typeCheckSpec = do
  it "type check" $
    checkSample program pred
  where
    typeOfAssignExpr stmt  = 
      let (AST.Statement (AST.AssignStmt (AST.Assignment _ _ (Just expr) _)) _) = stmt
      in view #tpe expr
    pred program =
      let (Right p) = Parser.parse program
          (Right (AST.ASTRoot imports vars methods, st)) = Semantic.analyze p
          stmts = view #stmts $ view #block $ head methods
          AST.Statement (AST.AssignStmt (AST.Assignment _ _ (Just expr) _)) _ = head stmts
          tpe = view #tpe expr
       in typeOfAssignExpr (stmts!!0) == AST.IntType &&
          typeOfAssignExpr (stmts!!1) == AST.BoolType && 
          typeOfAssignExpr (stmts!!2) == AST.BoolType 

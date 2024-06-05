{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module ParserSpec where

import Control.Lens (view)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Either (isRight)
import Parser (Expr (choicePredExpr))
import qualified Parser
import qualified Util.SourceLoc as SL
import Test.Hspec
import Util

spec :: Spec
spec = do
  describe "parser" $ do
    parseTopLevel

parseTopLevel :: SpecWith ()
parseTopLevel = do
  it "parse" $
    checkSample "test/samples/parser/hello_world.dcf" pred
  where
    pred program =
      let (Right p) = Parser.parse program
          main = SL.unLoc (head (Parser.methodDecls p))
          block = Parser.block main
          stmts = Parser.blockStatements block
          printCall = SL.unLoc $ stmts !! 0
          methodName = Parser.methodName $ Parser.methodCallStatement printCall
       in length (Parser.importDecls p) == 1
            && length (Parser.fieldDecls p) == 0
            && length (Parser.methodDecls p) == 1
            && Parser.methodId main == "main"
            && methodName == "print"

parseChoiceExpr :: SpecWith ()
parseChoiceExpr = do
  it "parse" $
    checkSample "test/samples/parser/choice_expr.dcf" pred
  where
    getChoiceExpr stmt =
      let (Parser.AssignStatement {Parser.assignExpr = (Parser.AssignExpr {Parser.assignSourceExpr = expr})}) = SL.unLoc stmt
       in SL.unLoc expr
    pred program =
      let (Right p) = Parser.parse program
          main = SL.unLoc (head (Parser.methodDecls p))
          block = Parser.block main
          stmts = Parser.blockStatements block
          choiceExpr = getChoiceExpr $ stmts !! 0
       in case choiceExpr of
            (Parser.ChoiceExpr _ _ (SL.LocatedAt _ (Parser.ChoiceExpr {}))) -> True
            _ -> False

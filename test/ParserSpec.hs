{-# LANGUAGE OverloadedStrings #-}

module ParserSpec where

import Data.ByteString.Lazy (ByteString)
import Data.Either (isRight)
import qualified Parser
import SourceLoc (unLocate)
import qualified SourceLoc as SL
import Test.Hspec

spec :: Spec
spec = do
  describe "parser" $ do
    parseTopLevel

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

parseTopLevel :: SpecWith ()
parseTopLevel = do
  it "parse" $
    let (Right p) = Parser.parse program
        main = unLocate (head (Parser.methodDecls p))
        block = Parser.block main
        stmts = Parser.blockStatements block
        printCall = unLocate $ stmts!!1
        methodName = Parser.methodName $ Parser.methodCallStatement printCall
     in length (Parser.importDecls p) == 2
          && length (Parser.fieldDecls p) == 3
          && length (Parser.methodDecls p) == 1
          && Parser.methodId main == "main"
          && methodName == "print"

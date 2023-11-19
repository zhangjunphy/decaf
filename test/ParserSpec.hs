{-# LANGUAGE OverloadedStrings #-}

module ParserSpec where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Either (isRight)
import qualified Parser
import SourceLoc (unLocate)
import qualified SourceLoc as SL
import Test.Hspec
import Util

spec :: Spec
spec = do
  describe "parser" $ do
    parseTopLevel

program :: String
program = "test/samples/hello_world.dcf"

parseTopLevel :: SpecWith ()
parseTopLevel = do
  it "parse" $
    checkSample program pred
  where
    pred program =
      let (Right p) = Parser.parse program
          main = unLocate (head (Parser.methodDecls p))
          block = Parser.block main
          stmts = Parser.blockStatements block
          printCall = unLocate $ stmts !! 0
          methodName = Parser.methodName $ Parser.methodCallStatement printCall
       in length (Parser.importDecls p) == 1
            && length (Parser.fieldDecls p) == 0
            && length (Parser.methodDecls p) == 1
            && Parser.methodId main == "main"
            && methodName == "print"

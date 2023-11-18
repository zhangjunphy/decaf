{-# LANGUAGE OverloadedStrings #-}

module Parser.ParserSpec where

import Data.ByteString.Lazy (ByteString)
import Data.Either (isRight)
import Scanner
import qualified SourceLoc as SL
import Test.Hspec

spec :: Spec
spec = do
  describe "scanner" $ do
    scanSingleElement
    scanExpr
    scanStmt
    scanFunction

getTokens :: [Either String (SL.Located Token)] -> [Token]
getTokens = fmap $ \(Right (SL.LocatedAt _ t)) -> t

compareTokenStream :: ByteString -> [Token] -> Bool
compareTokenStream inp toks =
  let res = Scanner.scan inp
   in getTokens res == toks

scanSingleElement :: SpecWith ()
scanSingleElement =
  do
    it "single element" $
      compareTokenStream "a" [Identifier "a"]
        && compareTokenStream "1" [IntLiteral "1"]
        && compareTokenStream "()" [LParen, RParen]
        && compareTokenStream "/* comment */" []

scanExpr :: SpecWith ()
scanExpr = do
  it "expr" $
    compareTokenStream "1 > 0" [IntLiteral "1", RelationOp ">", IntLiteral "0"]
      && compareTokenStream "a + b" [Identifier "a", ArithmeticOp "+", Identifier "b"]
      && compareTokenStream "y[z]" [Identifier "y", LBrack, Identifier "z", RBrack]

scanStmt :: SpecWith ()
scanStmt = do
  it "statement" $
    compareTokenStream "a = a * 1" [Identifier "a", AssignOp, Identifier "a", ArithmeticOp "*", IntLiteral "1"]
      && compareTokenStream "print(\"hello\")" [Identifier "print", LParen, StringLiteral "hello", RParen]

scanFunction :: SpecWith ()
scanFunction = do
  it "function" $
    compareTokenStream
      "int func() \n{a = b;}"
      [ Keyword "int",
        Identifier "func",
        LParen,
        RParen,
        LCurly,
        Identifier "a",
        AssignOp,
        Identifier "b",
        Semicolon,
        RCurly
      ]

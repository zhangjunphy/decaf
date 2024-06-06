{-# LANGUAGE OverloadedStrings #-}

module LexerSpec where

import Data.ByteString.Lazy (ByteString)
import Data.Either (isLeft)
import Lexer
import qualified Util.SourceLoc as SL
import Test.Hspec

spec :: Spec
spec = do
  describe "lexer" $ do
    scanSingleElement
    scanExpr
    scanStmt
    scanFunction
    scannerLoc

getTokens :: [Either String (SL.Located Token)] -> [Token]
getTokens = fmap $ \(Right (SL.LocatedAt _ t)) -> t

compareTokenStream :: ByteString -> [Token] -> Bool
compareTokenStream inp toks =
  let res = scan inp
   in getTokens res == toks

shouldErrorOut :: ByteString -> Bool
shouldErrorOut inp =
  let res = scan inp
   in length res == 1 && isLeft (head res)

checkLoc :: ByteString -> Int -> SL.Range -> Bool
checkLoc inp idxEle range =
  let res = scan inp
      (Right (SL.LocatedAt pos _)) = res !! idxEle
   in pos == range

scanSingleElement :: SpecWith ()
scanSingleElement =
  do
    it "elements" $
      compareTokenStream "a" [Identifier "a"]
        && compareTokenStream "1" [IntLiteral "1"]
        && compareTokenStream "()" [LParen, RParen]
        && compareTokenStream "/* comment */" []
        && compareTokenStream "/* line1 \n line2 \t \n line3 */" []
        && compareTokenStream "//" []
        && compareTokenStream "3 >= 2" [IntLiteral "3", RelationOp ">=", IntLiteral "2"]
        && compareTokenStream "if else for" [Keyword "if", Keyword "else", Keyword "for"]
        && compareTokenStream "\"this\t is\\\\ a string\"" [StringLiteral "this\t is\\ a string"]

scanError :: SpecWith ()
scanError = do
  it "should error" $
    shouldErrorOut "\f"
      && shouldErrorOut "|"
      && shouldErrorOut "|||"

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

scannerLoc :: SpecWith ()
scannerLoc = do
  it "location check" $
    checkLoc "int func() \n{a = b;}" 1 
      (SL.Range (SL.Posn 4 0 4) (SL.Posn 8 0 8))
    && checkLoc "int func() \n{a = b;}" 5
      (SL.Range (SL.Posn 13 1 1) (SL.Posn 14 1 2))

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

module LexerSpec where

import Data.ByteString.Lazy (ByteString)
import Data.Either (isLeft)
import Lexer
import qualified Util.SourceLoc as SL
import Test.Hspec
import Types

spec :: Spec
spec = do
  describe "lexer" $ do
    scanSingleElement
    scanExpr
    scanStmt
    scanFunction
    scannerLoc

getTokens :: Either [CompileError] [SL.Located Token] -> [Token]
getTokens (Left _) = [] 
getTokens (Right toks) = fmap SL.unLoc toks

scanTokens :: ByteString -> [Token]
scanTokens inp = getTokens $ scan inp

compareTokenStream :: ByteString -> [Token] -> Bool
compareTokenStream inp toks = scanTokens inp == toks

shouldErrorOut :: ByteString -> Bool
shouldErrorOut inp =
  case scan inp of
    Right _ -> False
    Left errs -> not $ null errs

checkLoc :: ByteString -> Int -> SL.Range -> Bool
checkLoc inp idxEle range =
  let (Right res) = scan inp
      (SL.LocatedAt pos _) = res !! idxEle
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

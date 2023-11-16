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

scanExpr :: SpecWith ()
scanExpr = do
  it "expr" $
    compareTokenStream "1 > 0" [IntLiteral "1", RelationOp ">", IntLiteral "0"]

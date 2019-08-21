module Parser.ParserSpec where

import           Test.Hspec

spec :: Spec
spec = do
  describe "test" $ do
    it "test" $
      abs 1 `shouldBe` 1

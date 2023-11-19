module Util where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import Test.Hspec (Expectation, shouldReturn)

checkSample :: String -> (ByteString -> Bool) -> Expectation
checkSample sample pred =
  let predIO = do
        program <- BL.readFile sample
        return $ pred program
   in predIO `shouldReturn` True

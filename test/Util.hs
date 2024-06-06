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

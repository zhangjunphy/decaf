-- Constants -- Definitions for some constants
-- Copyright (C) 2018 Jun Zhang <zhangjunphy[at]gmail[dot]com>
--
-- This file is a part of decafc.
--
-- decafc is free software: you can redistribute it and/or modify it under the
-- terms of the MIT (X11) License as described in the LICENSE file.
--
-- decafc is distributed in the hope that it will be useful, but WITHOUT ANY
-- WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE.  See the X11 license for more details.

{-# LANGUAGE OverloadedStrings #-}

module Constants
where

import Data.Text (Text)

globalScopeID :: Int
globalScopeID = 0

topLevelLabel :: Text
topLevelLabel = "root"

mainMethodName :: Text
mainMethodName = "main"
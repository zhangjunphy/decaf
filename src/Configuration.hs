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
module Configuration
  ( Configuration,
    input,
    target,
    debug,
    opt,
    outputFileName,
    defaultConfiguration,
    CompilerStage (..),
    OptimizationSpecification (..),
    OptimizationName (..),
  )
where

import Configuration.Types
  ( CompilerStage (..),
    Configuration (..),
    OptimizationName (..),
    OptimizationSpecification (..),
    debug,
    defaultConfiguration,
    explicitTarget,
    input,
    opt,
    outputFileName,
  )
import Data.Maybe (fromMaybe)

--------------------------- The configuration type ----------------------------
{- 'input', 'debug', 'opt', and 'outputFileName' are fine accessor functions.
'target', on the other hand, is a bit special. -}

target :: Configuration -> CompilerStage
target conf = fromMaybe defaultTarget $ explicitTarget conf
  where
    defaultTarget = Parse -- this will change as the course proceeds

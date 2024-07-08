-- Semantic -- Decaf IR generator and semantic checker
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
module CodeGen (buildLLVMIR) where

import CodeGen.LLVMGen
import CFG (SingleFileCFG)
import CodeGen.LLVMIR (Module)
import Types (CompileError)
import Control.Monad.Except (runExceptT)
import Control.Monad.State (runState)

buildLLVMIR :: SingleFileCFG -> Either [CompileError] Module
buildLLVMIR cfg =
  let genLLVM = genLLVMIR cfg
      (res, _) = (runState $ runExceptT $ runLLVMGen genLLVM) LLVMGenState
  in
    case res of
      Left err -> Left [err]
      Right mod -> Right mod 

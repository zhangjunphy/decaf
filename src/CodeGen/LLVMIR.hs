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

-- This should be a straightforward translation from CFG into LLVM IR

module CodeGen.LLVMIR where

import Data.Text (Text)

data Module = Module 
  { globals :: ![Global]
  , functions :: ![Function]
  }

data Global = Global
  { name :: !Text
  , tpe :: !Type
  }

data Function = Function
  { name :: !Text
  , arguments :: ![Argument]
  , basicBlocks :: ![BasicBlock]
  }

data Argument = Argument
  { name :: !Text
  , tpe :: !Type
  }

data Type =
  VoidType
  | IntType !Int
  | PointerType !Type
  | ArrayType !Type !Int

data BasicBlock = BasicBlock
  { label :: !Text
  , instructions :: ![Instruction]
  }

data Instruction = 
  Terminator !TermInst
  | Binary !BinaryInst
  | MemAccess !MemAccInst
  | Phi
  | Select
  | Call

data TermInst =
  Ret
  | Br

data BinaryInst =
  Add
  | Sub
  | Mul
  | SDiv

data MemAccInst =
  Alloca
  | Load
  | Store

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

import Data.Generics.Labels
import Data.Int (Int64)
import Data.Text (Text)
import GHC.Generics (Generic)
import Types (VID, Name)
import Util.SourceLoc qualified as SL

type Label = Text

data Module = Module
  { globals :: ![Global],
    functions :: ![Function]
  }

data Global = Global
  { name :: !Text,
    tpe :: !Type
  }

data Function = Function
  { name :: !Text,
    arguments :: ![Argument],
    basicBlocks :: ![BasicBlock]
  }

data Argument = Argument
  { name :: !Text,
    tpe :: !Type
  }

data Type
  = VoidType
  | IntType !Int
  | PointerType !Type
  | ArrayType !Type !Int

data BasicBlock = BasicBlock
  { label :: !Text,
    instructions :: ![Instruction]
  }

data Var = Var
  { id :: !VID,
    tpe :: !Type,
    loc :: !SL.Range
  }
  deriving (Generic)

data Value
  = IntImm !Type !Int64
  | Variable !Var

data Instruction
  = Terminator !TermInst
  | Binary !BinaryInst
  | MemAccess !MemAccInst
  | Assignment !Var !Value
  | Phi !Var ![(Var, Label)]
  | Select !Value !Value !Value
  | Call !Var !Type !Name ![Value]

data TermInst
  = Ret !Value !Type
  | BrUncon !Label
  | BrCon !Value !Label !Label

data BinaryInst
  = Add !Var !Type !Value !Value
  | Sub !Var !Type !Value !Value
  | Mul !Var !Type !Value !Value
  | SDiv !Var !Type !Value !Value

data MemAccInst
  = Alloca !Type !Int
  | Load !Type !Value
  | Store !Type !Value !Value

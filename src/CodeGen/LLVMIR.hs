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
import Formatting
import Control.Lens (use, uses, view, (%=), (%~), (&), (+=), (.=), (.~), (^.), _1, _2, _3)
import qualified Data.Text as Text

newtype Label = Label Int

instance Show Label where
  show (Label t) = formatToString ("l" % int) t

data Module = Module
  { globals :: ![Global],
    functions :: ![Function]
  }

instance Show Module where 
  show (Module globals functions) = formatToString (intercalated "\n" shown % "\n" % intercalated "\n" shown) globals functions

data Global = Global
  { name :: !Text,
    tpe :: !Type
  }

instance Show Global where
  show (Global name tpe) = formatToString ("@" % stext %+ "=" %+ "global" %+ shown) name tpe

data Function = Function
  { name :: !Text,
    returnType :: !Type,
    arguments :: ![Argument],
    basicBlocks :: ![BasicBlock]
  }

instance Show Function where
  show (Function name ret args bbs) =
    formatToString
      ("define" %+ shown %+ "@" % stext % "(" % intercalated ", " shown % ") {\n" % intercalated "\n" shown % "\n" % "}")
      ret name args bbs

data Argument = Argument
  { name :: !Text,
    tpe :: !Type
  }

instance Show Argument where
  show (Argument name tpe) = formatToString (shown %+ stext) tpe name

data Type
  = VoidType
  | IntType !Int
  | PointerType !Type
  | ArrayType !Type !Int

instance Show Type where
  show VoidType = "void"
  show (IntType n) = formatToString ("i" % int) n
  show (PointerType tpe) = "ptr"
  show (ArrayType tpe n) = "ptr"

arrayType :: Type -> Text
arrayType (ArrayType tpe n) = sformat ("<" % int %+ "x" %+ shown % ">") n tpe
arrayType t = Text.pack $ show t

data BasicBlock = BasicBlock
  { label :: !Label,
    instructions :: ![Instruction]
  }

instance Show BasicBlock where
  show (BasicBlock label insts) = 
    formatToString (shown % ":\n" % intercalated "\n" shown) label insts

data Var = Var
  { id :: !VID,
    tpe :: !Type,
    loc :: !SL.Range
  }
  deriving (Generic)

formatVar :: Format r (Var -> r)
formatVar = "%v" % viewed #id int

instance Show Var where
  show = formatToString formatVar 

data Value
  = IntImm !Type !Int64
  | Variable !Var

valueType :: Value -> Type
valueType (IntImm tpe _) = tpe
valueType (Variable var) = var ^. #tpe

instance Show Value where
  show (IntImm tpe n) = formatToString int n
  show (Variable v) = formatToString formatVar v

valueWithType :: Format r (Value -> r)
valueWithType = later $ \case
  (IntImm tpe n) -> bformat (shown %+ shown) tpe n
  (Variable v) -> bformat (shown %+ formatVar) (v ^. #tpe) v

data CondCodes
  = EQL
  | NEQ
  | SGT
  | SGE
  | SLT
  | SLE
  deriving (Eq)

instance Show CondCodes where
  show EQL = "eq"
  show NEQ = "ne"
  show SGT = "sgt"
  show SGE = "sge"
  show SLT = "slt"
  show SLE = "sle"

data Instruction
  = Terminator !TermInst
  | Binary !BinaryInst
  | BitBinary !BitwiseBinaryInst
  | MemAccess !MemAccInst
  | ICmp !Var !CondCodes !Type !Value !Value
  | Assignment !Var !Value
  | Phi !Var !Type ![(Var, Label)]
--  | Select !Value !Type !Value !Value
  | Call !Var !Type !Name ![Value]

instance Show Instruction where
  show (Terminator t) = show t
  show (Binary b) = show b
  show (BitBinary b) = show b
  show (MemAccess m) = show m
  show (ICmp var cc tpe v1 v2) = formatToString (formatVar %+ "=" %+ "icmp" %+ shown %+ shown %+ shown %+ shown) var cc tpe v1 v2
  show (Assignment var val) = formatToString (formatVar %+ "=" %+ shown %+ shown) var (valueType val) val
  show (Phi var tpe preds) = formatToString (formatVar %+ "= phi" %+ shown %+ intercalated ", " ("[" %+ viewed _1 shown % ", %" <> viewed _2 shown %+ "]")) var tpe preds
  show (Call var VoidType name args) = formatToString
    ("call" %+ shown %+ "@" % stext % "(" % intercalated ", " valueWithType % ")")
    VoidType name args
  show (Call var tpe name args) = formatToString
    (formatVar %+ "=" %+ "call" %+ shown %+ "@" % stext % "(" % intercalated ", " valueWithType % ")")
    var tpe name args

data TermInst
  = Ret !Value !Type
  | BrUncon !Label
  | BrCon !Value !Label !Label

instance Show TermInst where
  show (Ret val tpe) = formatToString ("ret" %+ shown %+ shown) tpe val
  show (BrUncon label) = formatToString ("br label %" % shown) label
  show (BrCon val l1 l2) =
    formatToString ("br" %+ shown %+ ", label %" % shown %+ ", label %" % shown) val l1 l2

data BinaryInst
  = Add !Var !Type !Value !Value
  | Sub !Var !Type !Value !Value
  | Mul !Var !Type !Value !Value
  | SDiv !Var !Type !Value !Value

instance Show BinaryInst where
  show (Add var tpe v1 v2) = formatToString (formatVar %+ "=" %+ "add" %+ shown %+ shown % ", " % shown)
    var tpe v1 v2
  show (Sub var tpe v1 v2) = formatToString (formatVar %+ "=" %+ "sub" %+ shown %+ shown % ", " % shown)
    var tpe v1 v2
  show (Mul var tpe v1 v2) = formatToString (formatVar %+ "=" %+ "mul" %+ shown %+ shown % ", " % shown)
    var tpe v1 v2
  show (SDiv var tpe v1 v2) = formatToString (formatVar %+ "=" %+ "sdiv" %+ shown %+ shown % ", " % shown)
    var tpe v1 v2

data BitwiseBinaryInst
  = And !Var !Type !Value !Value
  | Or !Var !Type !Value !Value

instance Show BitwiseBinaryInst where
  show (And var tpe v1 v2) = formatToString (formatVar %+ "=" %+ "and" %+ shown %+ shown % ", " % shown)
    var tpe v1 v2
  show (Or var tpe v1 v2) = formatToString (formatVar %+ "=" %+ "or" %+ shown %+ shown % ", " % shown)
    var tpe v1 v2

data MemAccInst
  = Alloca !Var !Type !Int64
  | Load !Var !Type !Value
  | Store !Type !Value !Value
  | StoreVec !Type ![(Type, Value)] !Value

instance Show MemAccInst where
  show (Alloca var tpe n) = formatToString (formatVar %+ "= alloca " % shown % ", " % shown %+ int)
    var tpe tpe n
  show (Load var tpe val) = formatToString (formatVar %+ "= load " % shown % ", " % "ptr" %+ shown)
    var tpe val 
  show (Store tpe val ptr) = formatToString ("store" %+ shown %+ shown % ", ptr " % shown) tpe val ptr
  show (StoreVec tpe vals ptr) = formatToString
    ("store" %+ stext %+ "<" % intercalated ", " (viewed _1 shown <%+> viewed _2 shown) % ">" %+ ", ptr " % shown)
    (arrayType tpe) vals ptr

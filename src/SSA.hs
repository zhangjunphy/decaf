-- SSA -- SSA Form Low Level IR
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
module SSA where

import AST (ArithOp, AssignOp, ChoiceOp, CondOp, EqOp, NegOp, NotOp, RelOp, Type)
import AST qualified
import Control.Monad.State
import Data.Text (Text)
import Util.SourceLoc qualified as SL
import Types

data Var = Var
  { id :: VID,
    tpe :: Type,
    astDecl :: Maybe AST.FieldDecl,
    loc :: Maybe SL.Range
  }
  deriving (Show)

data VarOrImm
  = BoolImm Bool
  | IntImm Int
  | StringImm Text
  | Variable Var
  deriving (Show)

type VarList = [Var]

data SSA
  = Assignment {dst :: Var, src :: VarOrImm}
  | MethodCall {dst :: Var, arguments :: [Var]}
  | Return {ret :: Var}
  | ArrayDeref {dst :: Var, arr :: Var, idx :: VarOrImm}
  | Arith {dst :: Var, arithOp :: ArithOp, opl :: VarOrImm, opr :: VarOrImm}
  | Rel {dst :: Var, relOp :: RelOp, opl :: VarOrImm, opr :: VarOrImm}
  | Cond {dst :: Var, condOp :: CondOp, opl :: VarOrImm, opr :: VarOrImm}
  | Eq {dst :: Var, eqOp :: EqOp, opl :: VarOrImm, opr :: VarOrImm}
  | Neg {dst :: Var, negOp :: NegOp, oprand :: VarOrImm}
  | Not {dst :: Var, notOp :: NotOp, oprand :: VarOrImm}
  | Choice {dst :: Var, choiceOp :: ChoiceOp, pred :: VarOrImm, opl :: VarOrImm, opr :: VarOrImm}
  | Len {dst :: Var, arr :: Var}
  deriving (Show)

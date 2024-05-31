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
import Data.Int (Int64)
import Data.Text (Text)
import Control.Lens (_1, _2)
import Formatting
import Types
import Util.SourceLoc qualified as SL

data Var = Var
  { id :: !VID,
    tpe :: !Type,
    astDecl :: !(Maybe (Either AST.Argument AST.FieldDecl)),
    loc :: !SL.Range
  }

type VarList = [Var]

type Label = Text

instance Show Var where
  show var@Var {id = id} = formatToString ("v" % int) id

data VarOrImm
  = BoolImm !Bool
  | IntImm !Int64
  | CharImm !Char
  | StringImm !Text
  | Variable !Var

instance Show VarOrImm where
  show (BoolImm True) = "true"
  show (BoolImm False) = "false"
  show (IntImm val) = formatToString int val
  show (CharImm val) = formatToString ("'" % char % "'") val
  show (StringImm val) = formatToString ("\"" % stext % "\"") val
  show (Variable Var {id = id}) = formatToString ("v" % int) id

data SSA
  = Assignment {dst :: !Var, src :: !VarOrImm}
  | MethodCall {dst :: !Var, name :: !Name, arguments :: ![Var]}
  | Return {ret :: !Var}
  | Load {dst :: !Var, ptr :: !VarOrImm}
  | Store {ptr :: !VarOrImm, src :: !VarOrImm}
  | Arith {dst :: !Var, arithOp :: !ArithOp, opl :: !VarOrImm, opr :: !VarOrImm}
  | Rel {dst :: !Var, relOp :: !RelOp, opl :: !VarOrImm, opr :: !VarOrImm}
  | Cond {dst :: !Var, condOp :: !CondOp, opl :: !VarOrImm, opr :: !VarOrImm}
  | Eq {dst :: !Var, eqOp :: !EqOp, opl :: !VarOrImm, opr :: !VarOrImm}
  | Neg {dst :: !Var, negOp :: !NegOp, oprand :: !VarOrImm}
  | Not {dst :: !Var, notOp :: !NotOp, oprand :: !VarOrImm}
  | Choice {dst :: !Var, choiceOp :: !ChoiceOp, pred :: !VarOrImm, opl :: !VarOrImm, opr :: !VarOrImm}
  | Len {dst :: !Var, arr :: !Var}
  | Phi {dst :: !Var, predecessors :: ![(Var, BBID)]}
  | BrUncon { target :: !Label}
  | BrCon {pred :: !VarOrImm, targetT :: !Label, targetF :: !Label}

ppVars :: Format r ([Var] -> r)
ppVars = intercalated ", " shown

ppPhiPreds :: Format r ([(Var, BBID)] -> r)
ppPhiPreds = intercalated ", " showPredPair
  where
    showPredPair :: Format r ((Var, BBID) -> r)
    showPredPair = "[" % viewed _1 shown % ", %" <> viewed _2 int % "]" 

instance Show SSA where
  show (Assignment dst src) = formatToString (shown %+ "=" %+ shown) dst src
  show (MethodCall dst name arguments) = formatToString (shown %+ "=" %+ stext % "(" % ppVars % ")") dst name arguments
  show (Return ret) = formatToString ("return" %+ shown) ret
  show (Load dst ptr) = formatToString (shown %+ "= &" % shown) dst ptr
  show (Store ptr src) = formatToString ("&" % shown %+ "=" %+ shown) ptr src
  show (Arith dst op opl opr) = formatToString (shown %+ "=" %+ shown %+ shown %+ shown) dst opl op opr
  show (Rel dst op opl opr) = formatToString (shown %+ "=" %+ shown %+ shown %+ shown) dst opl op opr
  show (Cond dst op opl opr) = formatToString (shown %+ "=" %+ shown %+ shown %+ shown) dst opl op opr
  show (Eq dst op opl opr) = formatToString (shown %+ "=" %+ shown %+ shown %+ shown) dst opl op opr
  show (Neg dst op opd) = formatToString (shown %+ "=" %+ shown % shown) dst op opd
  show (Not dst op opd) = formatToString (shown %+ "=" %+ shown % shown) dst op opd
  show (Choice dst op pred opl opr) = formatToString (shown %+ "=" %+ shown %+ "?" %+ shown %+ ":" %+ shown) dst pred opl opr
  show (Len dst arr) = formatToString (shown %+ "= len(" % shown % ")") dst arr
  show (Phi dst preds) = formatToString (shown %+ "= phi" %+ ppPhiPreds) dst preds

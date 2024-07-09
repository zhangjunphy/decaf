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

-- SSA -- SSA Form Low Level IR
module SSA (Locality (..), Var (..), VarList, Label, VarOrImm (..), SSA (..)) where

import AST (ArithOp, AssignOp, CondOp, EqOp, NegOp, NotOp, RelOp, Type)
import AST qualified
import Control.Lens (use, uses, view, (%=), (%~), (&), (+=), (.=), (.~), (^.), _1, _2, _3)
import Control.Monad.State
import Data.Generics.Labels
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as Text
import Formatting
import GHC.Generics (Generic)
import Types
import Util.SourceLoc qualified as SL

data Locality
  = Global
  | Local

data Var = Var
  { id :: !VID,
    tpe :: !Type,
    astDecl :: !(Maybe (Either AST.Argument AST.FieldDecl)),
    loc :: !SL.Range,
    locality :: !Locality
  }
  deriving (Generic)

type VarList = [Var]

type Label = Text

instance Show Var where
  show var@Var {id = id} = formatToString ("v" % int) id

data VarOrImm
  = BoolImm !Bool
  | IntImm !Int64
  | CharImm !Char
  | StringImm !Text
  | PtrImm !Int64
  | Variable !Var

escapeChar :: Char -> Text
escapeChar = \case
  '\\' -> "\\\\\\\\"
  '\n' -> "\\\\n"
  '\t' -> "\\\\t"
  '\'' -> "\\\'"
  '"' -> "\\\\\\\""
  '\0' -> "\\\\0"
  c -> Text.singleton c

escape :: Text -> Text
escape = Text.concatMap escapeChar

instance Show VarOrImm where
  show (BoolImm True) = "true"
  show (BoolImm False) = "false"
  show (IntImm val) = formatToString int val
  show (CharImm val) = formatToString ("'" % stext % "'") $ escapeChar val 
  show (PtrImm val) = formatToString prefixHex val 
  show (StringImm val) = formatToString ("\\\"" % stext % "\\\"") $ escape val
  show (Variable Var {id = id}) = formatToString ("v" % int) id

data SSA
  = Assignment {dst :: !Var, src :: !VarOrImm}
  | MethodCall {dst :: !Var, name :: !Name, arguments :: ![Var]}
  | Return {ret :: !(Maybe VarOrImm)}
  | Alloca {dst :: !Var, tpe :: !AST.Type, sz :: !(Maybe Int64)}
  | Load {dst :: !Var, ptr :: !VarOrImm}
  | Store {ptr :: !VarOrImm, src :: !VarOrImm}
  | Arith {dst :: !Var, arithOp :: !ArithOp, opl :: !VarOrImm, opr :: !VarOrImm}
  | Rel {dst :: !Var, relOp :: !RelOp, opl :: !VarOrImm, opr :: !VarOrImm}
  | Cond {dst :: !Var, condOp :: !CondOp, opl :: !VarOrImm, opr :: !VarOrImm}
  | Eq {dst :: !Var, eqOp :: !EqOp, opl :: !VarOrImm, opr :: !VarOrImm}
  | Neg {dst :: !Var, negOp :: !NegOp, oprand :: !VarOrImm}
  | Not {dst :: !Var, notOp :: !NotOp, oprand :: !VarOrImm}
  | Phi {dst :: !Var, predecessors :: ![(Var, BBID)]}
  | AllocaStr {dst :: !Var, content :: !Text, tpe :: !AST.Type}
  | BrUncon {target :: !Label}
  | BrCon {pred :: !VarOrImm, targetT :: !Label, targetF :: !Label}
  deriving (Generic)

ppVarWithType :: Format r (Var -> r)
ppVarWithType = viewed #tpe shown <%+> shown

ppVars :: Format r ([Var] -> r)
ppVars = intercalated ", " shown

ppPhiPreds :: Format r ([(Var, BBID)] -> r)
ppPhiPreds = intercalated ", " showPredPair
  where
    showPredPair :: Format r ((Var, BBID) -> r)
    showPredPair = "[" % viewed _1 shown % ", %" <> viewed _2 int % "]"

instance Show SSA where
  show (Assignment dst src) = formatToString (ppVarWithType %+ "=" %+ shown) dst src
  show (MethodCall dst name arguments) = formatToString (ppVarWithType %+ "=" %+ stext % "(" % ppVars % ")") dst name arguments
  show (Return Nothing) = "return"
  show (Return (Just val)) = formatToString ("return" %+ shown) val
  show (Load dst ptr) = formatToString (ppVarWithType %+ "= *" % shown) dst ptr
  show (Store ptr src) = formatToString ("*" % shown %+ "=" %+ shown) ptr src
  show (Alloca dst tpe Nothing) = formatToString (ppVarWithType %+ "= alloca" %+ shown) dst tpe
  show (Alloca dst tpe (Just sz)) = formatToString (ppVarWithType %+ "= alloca" %+ shown %+ int) dst tpe sz
  show (Arith dst op opl opr) = formatToString (ppVarWithType %+ "=" %+ shown %+ shown %+ shown) dst opl op opr
  show (Rel dst op opl opr) = formatToString (ppVarWithType %+ "=" %+ shown %+ shown %+ shown) dst opl op opr
  show (Cond dst op opl opr) = formatToString (ppVarWithType %+ "=" %+ shown %+ shown %+ shown) dst opl op opr
  show (Eq dst op opl opr) = formatToString (ppVarWithType %+ "=" %+ shown %+ shown %+ shown) dst opl op opr
  show (Neg dst op opd) = formatToString (ppVarWithType %+ "=" %+ shown % shown) dst op opd
  show (Not dst op opd) = formatToString (ppVarWithType %+ "=" %+ shown % shown) dst op opd
  show (Phi dst preds) = formatToString (ppVarWithType %+ "= phi" %+ ppPhiPreds) dst preds
  show (AllocaStr dst content tpe) = formatToString (ppVarWithType %+ "= string \\\"" % stext % "\\\"") dst (escape content)

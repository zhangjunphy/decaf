-- CFG -- Control Flow Graph with SSA nodes
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
module CFG where

import AST qualified
import CFG.PartialCFG qualified as PCFG
import Control.Lens (use, view, (%=), (%~), (+=), (.=), (.~), (^.))
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Functor ((<&>))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Text (Text)
import Formatting
import GHC.Generics (Generic)
import SSA
import Semantic qualified as SE
import Types
import Util.Graph qualified as G
import Util.SourceLoc qualified as SL

data VarBiMap = VarBiMap
  { varToSym :: Map VID (ScopeID, Name),
    symToVar :: Map (ScopeID, Name) VID
  }

addVarSym :: ScopeID -> Name -> VID -> VarBiMap -> VarBiMap
addVarSym sid name vid (VarBiMap varToSym symToVar) =
  let varToSym' = Map.insert vid (sid, name) varToSym
      symToVar' = Map.insert (sid, name) vid symToVar
   in VarBiMap varToSym' symToVar'

lookupVar :: VID -> VarBiMap -> Maybe (ScopeID, Name)
lookupVar vid VarBiMap {varToSym = m} = Map.lookup vid m

lookupSym :: ScopeID -> Name -> VarBiMap -> Maybe VID
lookupSym sid name VarBiMap {symToVar = m} = Map.lookup (sid, name) m

data Condition
  = Pred {pred :: VarOrImm}
  | Complement
  deriving (Show)

data BasicBlock = BasicBlock
  { bbid :: BBID,
    sid :: ScopeID,
    statements :: [SSA]
  }
  deriving (Generic, Show)

data CFGNode = CFGNode
  { bb :: BasicBlock
  }
  deriving (Generic, Show)

data CFGEdge
  = SeqEdge
  | CondEdge Condition
  deriving (Show)

type CFG = G.Graph BBID CFGNode CFGEdge

type CFGBuilder = G.GraphBuilder BBID CFGNode CFGEdge

data CFGState = CFGState
  { cfg :: CFG,
    scope :: ScopeID,
    vars :: VarList,
    symMap :: VarBiMap,
    exitVar :: Map BBID Var
  }
  deriving (Generic)

data CFGContext = CFGContext
  { symbolTables :: Map ScopeID SE.SymbolTable
  }
  deriving (Generic)

newtype CFGExcept = CFGExcept Text
  deriving (Show)

newtype CFGBuild a = CFGBuild
  { runCFGBuild ::
      ExceptT
        CFGExcept
        (ReaderT CFGContext (State CFGState))
        a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadError CFGExcept,
      MonadReader CFGContext,
      MonadState CFGState
    )

setScope :: ScopeID -> CFGBuild ()
setScope sid = do
  #scope .= sid

getVarSymbol :: Name -> CFGBuild AST.FieldDecl
getVarSymbol name = do
  sid <- use #scope
  sts <- view #symbolTables
  let st = fromJust $ Map.lookup sid sts
  return $ fromJust $ Map.lookup name (st ^. #variableSymbols)

newVar :: Maybe Name -> SL.Range -> AST.Type -> CFGBuild Var
newVar Nothing sl tpe = do
  vars <- use #vars
  let vid = length vars
  let var = Var vid tpe Nothing sl
  #vars .= vars ++ [var]
  return var
newVar (Just name) sl tpe = do
  vars <- use #vars
  let vid = length vars
  decl <- getVarSymbol name
  let var = Var vid tpe (Just decl) sl
  #vars .= vars ++ [var]
  return var

findVarOfSym :: Name -> CFGBuild (Maybe Var)
findVarOfSym name = do
  sid <- use #scope
  varBiMap <- use #symMap
  vars <- use #vars
  let vid = lookupSym sid name varBiMap
  return $ vid <&> (vars !!)

findVarOfSym' :: Name -> CFGBuild Var
findVarOfSym' name = do
  var <- findVarOfSym name
  case var of
    Nothing -> throwError $ CFGExcept $ sformat ("Unable to find variable " % stext) name
    Just v -> return v

-- buildCFG :: AST.ASTRoot -> CFGContext -> Either CFGExcept (Map Name CFG)
-- buildCFG (AST.ASTRoot _ _ methods) context = _

-- buildMethod :: PCFG.CFG -> CFGBuild ()
-- buildMethod g@(G.Graph nodes edges) = G.traverseM_ buildNode g

-- buildNode :: BBID -> PCFG.CFGNode -> CFGBuild ()
-- buildNode _ PCFG.CFGNode {bb = (PCFG.BasicBlock bbid sid stmts)} = do
--   cfg <- use #cfg
--   setScope sid
--   let bb = BasicBlock bbid sid _
--   _

-- buildStatement :: AST.Statement -> CFGBuild SSA
-- buildStatement (AST.AssignStmt (AST.Assignment (AST.Typed (AST.Location name Nothing def) tpe) op expr)) = do
--   _

-- buildExpr :: AST.Expr -> CFGBuild Var
-- buildExpr (AST.LocationExpr (AST.Location name Nothing def)) = do
--   var <- findVarOfSym name
--   case var of
--     Nothing -> throwError $ CFGExcept $ sformat ("Unable to find variable " % stext) name
--     Just var' -> return var'

-- buildLocation :: AST.Location -> CFGBuild (Maybe Var)
-- buildLocation (AST.Location name Nothing def) = findVarOfSym' name
-- buildLocation (AST.Location name (Just expr) def) = do
--   idxVar <- buildExpr expr
--   arrayVar <- findVarOfSym' name
--   let tpe = AST.typeOfDef def
--   _

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
import Control.Lens (use, view, (%=), (%~), (+=), (.=), (.~), (^.), _1)
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
import qualified Data.GraphViz.Types.Graph as G

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
    statements :: [SSA]
  }
  deriving (Generic)

initialState :: CFGState
initialState = CFGState G.empty 0 [] (VarBiMap Map.empty Map.empty) []

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

addSSA :: SSA -> CFGBuild ()
addSSA ssa = do
  stmts <- use #statements
  #statements .= stmts ++ [ssa]

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

buildCFG :: AST.ASTRoot -> CFGContext -> Either CFGExcept (Map Name CFG)
buildCFG root@(AST.ASTRoot _ _ methods) context =
  let pcfgs = PCFG.buildCFG root (PCFG.CFGContext $ context ^. #symbolTables)
  in
    case pcfgs of
      Left (PCFG.CFGExcept msg) -> Left $ CFGExcept msg
      Right cfgs -> let buildMethods = mapM buildMethod cfgs
                        runBuild = runState $ flip runReaderT context $
                          runExceptT $ runCFGBuild buildMethods
                    in runBuild initialState ^. _1

buildMethod :: PCFG.CFG -> CFGBuild CFG
buildMethod g@(G.Graph nodes edges) = do
  G.traverseM_ buildNode g
  use #cfg

buildNode :: BBID -> PCFG.CFGNode -> CFGBuild CFGNode
buildNode _ PCFG.CFGNode {bb = (PCFG.BasicBlock bbid sid stmts)} = do
  cfg <- use #cfg
  setScope sid
  forM_ stmts buildStatement
  stmts <- use #statements
  let bb = BasicBlock bbid sid stmts
  return $ CFGNode bb

buildStatement :: AST.Statement -> CFGBuild ()
buildStatement (AST.Statement (AST.AssignStmt (AST.Assignment (AST.Location name Nothing def tpe loc) op (Just expr) _)) _) = do
  dst <- newVar (Just name) loc tpe
  src <- buildExpr expr
  addSSA $ Assignment dst (Variable src)

buildExpr :: AST.Expr -> CFGBuild Var
buildExpr (AST.Expr (AST.LocationExpr location) tpe _) = buildLocation location

buildLocation :: AST.Location -> CFGBuild Var
buildLocation (AST.Location name Nothing def tpe loc) = findVarOfSym' name
buildLocation (AST.Location name (Just expr) def tpe loc) = do
  idx <- buildExpr expr
  array <- findVarOfSym' name
  dst <- newVar (Just name) loc tpe
  addSSA $ ArrayDeref dst array (Variable idx)
  return dst

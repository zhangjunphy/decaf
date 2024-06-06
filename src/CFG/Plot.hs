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

module CFG.Plot where

import AST qualified
import Semantic qualified as SE
import Data.Functor ( (<&>) )
import CFG.Types
import CFG.Build
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Map qualified as Map
import Formatting
import Types
import Util.Graph qualified as G

prettyPrintNode :: CFGNode -> Text
prettyPrintNode CFGNode {bb = BasicBlock {bbid = id, statements = stmts}} =
  let idText = [sformat ("id: " % int) id]
      segments = stmts <&> \s -> sformat shown s
   in Text.intercalate "\n" $ idText ++ segments

escape :: Text -> Text
escape =
  Text.concatMap
    ( \case
        '\\' -> "\\\\"
        '"' -> "\\\""
        c -> Text.singleton c
    )

prettyPrintEdge :: CFGEdge -> Text
prettyPrintEdge SeqEdge = ""
prettyPrintEdge (CondEdge (Pred var)) = sformat shown var
prettyPrintEdge (CondEdge Complement) = "otherwise"

generateDotPlot :: G.Graph BBID CFGNode CFGEdge -> Text
generateDotPlot G.Graph {nodes = nodes, edges = edges} =
  let preamble = "digraph G {\n"
      postamble = "}"
      nodeBoxes = Map.assocs nodes <&> uncurry nodeBox
      edgeLines =
        concatMap
          (\(from, tos) -> tos <&> uncurry (edgeLine from))
          $ Map.assocs edges
   in mconcat $ [preamble] ++ nodeBoxes ++ edgeLines ++ [postamble]
  where
    nodeBox idx d =
      sformat
        (int % " [shape=box, label=\"" % stext % "\"];\n")
        idx
        (escape (prettyPrintNode d))
    edgeLine from to d =
      sformat
        (int % " -> " % int % " [label=\"" % stext % "\"];\n")
        from
        to
        (escape (prettyPrintEdge d))

plot :: AST.ASTRoot -> SE.SemanticInfo -> Either [String] String
plot root si =
  let context = CFGContext si
      res = buildCFG root context
   in case res of
        Left (CFGExcept msg) -> Left [Text.unpack msg]
        Right cfgs -> Right $ Text.unpack $ mconcat $ Map.elems cfgs <&> generateDotPlot

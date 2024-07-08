{- Main -- main entry point
Copyright (C) 2020 J.Z. <zhangjunphy at gmail dot com>

This file is a part of decafc.

decafc is free software: you can redistribute it and/or modify it under the
terms of the MIT (X11) License as described in the LICENSE file.

decafc is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the X11 license for more details. -}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import CFG qualified
import Configuration (CompilerStage (..), Configuration)
import Configuration qualified
import Control.Exception (bracket)
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as B
import Data.Functor ((<&>))
import Data.List
import Data.Text (Text)
import Data.Text qualified as Text
import Formatting (sformat, shown, (%), (%+))
import GHC.IO.Handle (hDuplicate)
import Lexer qualified
import Parser qualified
import Semantic qualified
import System.Environment (getProgName)
import System.Exit qualified
import System.IO (IOMode (..), hClose, hPutStrLn, openFile, stderr, stdout)
import Text.Printf (printf)
import Types
import Util.CLI qualified as CLI
import Util.SourceLoc qualified as SL
import Prelude hiding (readFile)
import CodeGen (buildLLVMIR)

------------------------ Impure code: Fun with ExceptT ------------------------

main :: IO ()
main = do
  {- Compiler work can be split into three stages: reading input (impure),
  processing it (pure), and writing output (impure).  Of course, input might be
  malformed or there might be an error in processing.  Thus, it makes most
  sense to think of the compiler as having type ExceptT String IO [IO ()] --
  that is, computation might fail with a String or succeed with a series of IO
  actions. -}
  result <- runExceptT $ do
    -- Part I: Get input
    configuration <- ExceptT CLI.getConfiguration
    input <- readFile $ Configuration.input configuration
    -- Part II: Process it
    hoistEither $ process configuration input
  case result of
    -- Part III: Write output
    Left errorMessage -> fatal errorMessage
    Right actions -> sequence_ actions
  where
    hoistEither = ExceptT . return

readFile :: FilePath -> ExceptT String IO ByteString
readFile name = liftIO $ B.readFile name

fatal :: String -> IO ()
fatal message = do
  progName <- getProgName
  hPutStrLn stderr $ printf "%s: %s" progName message
  System.Exit.exitFailure

---------------------------- Pure code: Processing ----------------------------

{- Since our compiler only handles single files, the 'Configuration' struct
doesn't currently get passed through to the lexer and parser code.  (This may
change--one can see the lexer and parser as acting in a reader monad.)  The
big problem with this is that error messages generated in the lexer and
parser won't contain the file name--the file name has to get added in this
function. -}
mungeErrorMessage :: Configuration -> CompileError -> String
mungeErrorMessage configuration error =
  Configuration.input configuration ++ ":" ++ show error

{- The pure guts of the compiler convert input to output.  Exactly what output
they produce, though, depends on the configuration. -}
process :: Configuration -> ByteString -> Either String [IO ()]
process configuration input =
  -- Dispatch on the configuration, modifying error messages appropriately.
  let stageResult =
        case Configuration.target configuration of
          Scan -> scan input
          Parse -> parse input
          Cfg -> cfg input
          LLVM -> llvm input
          -- Inter -> irgen configuration input
          phase -> Left [CompileError Nothing $ sformat (shown %+ "not implemented") phase]
   in outputStageResult configuration stageResult

{- We have to interleave output to standard error (for errors) and standard
output or a file (for output), so we need to actually build an appropriate
set of IO actions. -}
outputStageResult :: Configuration -> Either [CompileError] String -> Either String [IO ()]
outputStageResult configuration resultOrErrors =
  Right
    [ case resultOrErrors of
        Left errors ->
          bracket openOutputHandle hClose $ \hOutput ->
            forM_ errors (hPutStrLn hOutput . mungeErrorMessage configuration)
        Right result ->
          bracket openOutputHandle hClose $ \hOutput ->
            hPutStrLn hOutput result
    ]
  where
    openOutputHandle =
      maybe (hDuplicate stdout) (`openFile` WriteMode) $
        Configuration.outputFileName configuration

scan :: ByteString -> Either [CompileError] String
scan input = Lexer.scan input <&> intercalate "\n" . fmap formatToken
  where
    formatToken (SL.LocatedAt (SL.Range (SL.Posn _ line _) _) tok) =
      unwords [show line, show tok]

parse :: ByteString -> Either [CompileError] String
parse input = do
  tree <- Parser.parse input
  (program, _) <- Semantic.analyze tree
  return $ show program

cfg :: ByteString -> Either [CompileError] String
cfg input = do
  tree <- Parser.parse input
  (program, semantic) <- Semantic.analyze tree
  CFG.plot program semantic

llvm :: ByteString -> Either [CompileError] String
llvm input = do
  tree <- Parser.parse input
  (program, semantic) <- Semantic.analyze tree
  fileCFG <- CFG.buildAndOptimize program semantic
  mod <- CodeGen.buildLLVMIR fileCFG
  return $ show mod

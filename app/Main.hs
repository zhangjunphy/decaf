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

import qualified CLI
import Configuration (CompilerStage (..), Configuration)
import qualified Configuration
import Control.Exception (bracket)
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import Data.Functor ((<&>))
import Data.List
import Data.Text (Text)
import GHC.IO.Handle (hDuplicate)
import qualified Parser
import qualified Scanner
import qualified Semantic
import System.Environment (getProgName)
import qualified System.Exit
import System.IO
  ( IOMode (..),
    hClose,
    hPutStrLn,
    openFile,
    stderr,
    stdout,
  )
import Text.Printf (printf)
import Prelude hiding (readFile)

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
doesn't currently get passed through to the scanner and parser code.  (This may
change--one can see the scanner and parser as acting in a reader monad.)  The
big problem with this is that error messages generated in the scanner and
parser won't contain the file name--the file name has to get added in this
function. -}
mungeErrorMessage :: Configuration -> Either String a -> Either String a
mungeErrorMessage configuration =
  ifLeft $ \msg -> Configuration.input configuration ++ ":" ++ msg
  where
    ifLeft f (Left v) = Left $ f v
    ifLeft _ (Right a) = Right a

{- The pure guts of the compiler convert input to output.  Exactly what output
they produce, though, depends on the configuration. -}
process :: Configuration -> ByteString -> Either String [IO ()]
process configuration input =
  -- Dispatch on the configuration, modifying error messages appropriately.
  case Configuration.target configuration of
    Scan -> scan configuration input
    Parse -> parse configuration input
    -- Inter -> irgen configuration input
    phase -> Left $ show phase <> " not implemented\n"

{- We have to interleave output to standard error (for errors) and standard
output or a file (for output), so we need to actually build an appropriate
set of IO actions. -}
outputStageResult :: Configuration -> Either [String] String -> Either String [IO ()]
outputStageResult configuration resultOrErrors =
  Right
    [ case resultOrErrors of
        Left errors ->
          bracket openOutputHandle hClose $ \hOutput ->
            forM_ errors (hPutStrLn hOutput)
        Right result ->
          bracket openOutputHandle hClose $ \hOutput ->
            hPutStrLn hOutput result
    ]
  where
    openOutputHandle =
      maybe (hDuplicate stdout) (`openFile` WriteMode) $
        Configuration.outputFileName configuration

scan :: Configuration -> ByteString -> Either String [IO ()]
scan configuration input =
  let tokensAndErrors =
        Scanner.scan input
          <&> mungeErrorMessage configuration
          <&> Scanner.formatTokenOrError
      tokens = intercalate "\n" $ tokensAndErrors >>= either (const []) return
      errors = tokensAndErrors >>= either return (const [])
      result = case errors of
        [] -> Right tokens
        _ -> Left errors
   in outputStageResult configuration result

parse :: Configuration -> ByteString -> Either String [IO ()]
parse configuration input =
  let irAndError = do
        tree <- Parser.parse input
        Semantic.runSemanticAnalysis tree
      result = case irAndError of
        Left exception -> Left [exception]
        Right (_, err, _) | not (null err) -> Left $ show <$> err
        Right (root, _, _) -> Right $ show root
  in outputStageResult configuration result

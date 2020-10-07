{- Main -- main entry point
Copyright (C) 2013, 2014  Benjamin Barenblat <bbaren@mit.edu>

This file is a part of decafc.

decafc is free software: you can redistribute it and/or modify it under the
terms of the MIT (X11) License as described in the LICENSE file.

decafc is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the X11 license for more details. -}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Prelude                    hiding (readFile)
import qualified Prelude

import           Control.Exception          (bracket)
import           Control.Monad              (forM_, guard, void, when)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import           Data.Either                (isRight, partitionEithers)
import           Debug.Trace
import           GHC.IO.Handle              (hDuplicate)
import           System.Environment         (getProgName)
import qualified System.Exit
import           System.IO                  (IOMode (..), hClose, hPutStrLn,
                                             openFile, stderr, stdout)
import           Text.Printf                (printf)
import           Text.Show.Pretty

import qualified CLI
import           Configuration              (CompilerStage (..), Configuration)
import qualified Configuration
import qualified IR
import qualified Parser
import qualified Scanner

import           Data.ByteString.Lazy       (ByteString)
import qualified Data.ByteString.Lazy       as B
import qualified Data.ByteString.Lazy.UTF8  as B (fromString, toString)


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
    Right actions     -> sequence_ actions
  where hoistEither = ExceptT . return

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
  ifLeft ((Configuration.input configuration ++ ":")++)
  where ifLeft f (Left v)  = Left $ f v
        ifLeft _ (Right a) = Right a

{- The pure guts of the compiler convert input to output.  Exactly what output
they produce, though, depends on the configuration. -}
process :: Configuration -> ByteString -> Either String [IO ()]
process configuration input =
  -- Dispatch on the configuration, modifying error messages appropriately.
  case Configuration.target configuration of
    Scan  -> scan configuration input
    Parse -> parse configuration input
    -- Inter -> irgen configuration input
    phase -> Left $ show phase ++ " not implemented\n"

{- We have to interleave output to standard error (for errors) and standard
output or a file (for output), so we need to actually build an appropriate
set of IO actions. -}
outputStageResult :: Configuration -> [Either String String] -> Either String [IO ()]
outputStageResult configuration resultAndErrors =
    Right [ bracket openOutputHandle hClose $ \hOutput ->
                forM_ resultAndErrors $ \resultOrError ->
                    case resultOrError of
                        Left err -> hPutStrLn hOutput err
                        Right r  -> hPutStrLn hOutput r
          ]
    where openOutputHandle = maybe (hDuplicate stdout) (`openFile` WriteMode) $
                             Configuration.outputFileName configuration

scan :: Configuration -> ByteString -> Either String [IO ()]
scan configuration input =
  let tokensAndErrors =
        Scanner.scan input |>
        map (mungeErrorMessage configuration) |>
        map Scanner.formatTokenOrError
  in outputStageResult configuration tokensAndErrors
  where v |> f = f v            -- like a Unix pipeline, but pure

parse :: Configuration -> ByteString -> Either String [IO ()]
parse configuration input = do
  let x = Parser.parse input
  let tree = mungeErrorMessage configuration x
  outputStageResult configuration [ppShow <$> tree]
  guard (isRight tree)
  let ir = IR.generate <$> tree
  let semantic = IR.runSemanticAnalysis <$> ir
  -- TODO: This does not work for now. We need to find a way to thread IO Monad into our
  -- SemanticState so that we can do IO operations here.
  outputStageResult configuration [ppShow <$> semantic]

  -- let (errors, tokens) = partitionEithers $ Scanner.alexMonadScan input
  -- -- If errors occurred, bail out.
  -- mapM_ (mungeErrorMessage configuration . Left) errors
  -- -- Otherwise, attempt a parse.
  -- -- void $ mungeErrorMessage configuration $ Parser.parse tokens
  -- -- comment the above line and uncomment the following two lines to print out your parse tree
  -- let x = Parser.parse tokens
  -- void $ mungeErrorMessage configuration $ trace (ppShow x) x
  -- Right []

-- | IR generator
-- irgen :: Configuration -> String -> Either String [IO ()]
-- irgen configuration input = do
--   let (errors, tokens) = partitionEithers $ Scanner.scan input
--   mapM_ (mungeErrorMessage configuration . Left) errors
--   let x = Parser.parse tokens
--   case x of
--     Left msg -> Left msg
--     Right ast -> Right $ [ print $ IR.generate $ ast]

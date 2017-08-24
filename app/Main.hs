module Main
  ( main
  ) where

import           Actions
import           Language.Haskell.Format
import           Language.Haskell.Format.Utilities  hiding (wasReformatted)
import           Language.Haskell.Source.Enumerator
import           OptionsParser                      as Options
import           Types

import           Control.Applicative
import           Control.Monad
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Options.Applicative.Extra          as OptApp
import           Pipes
import qualified Pipes.Prelude                      as P
import           System.Exit

main :: IO ()
main = do
  options <- execParser Options.parser
  formatter <- defaultFormatter
  reformatNeeded <- runFormatter formatter options
  if reformatNeeded
    then exitFailure
    else exitSuccess

runFormatter :: Formatter -> Options -> IO Bool
runFormatter formatter options =
  anyStrict
    (sourceChangedOrHasSuggestions options)
    (inputFiles >-> P.map reformat >-> P.mapM writeOutput)
  where
    anyStrict :: Monad m => (a -> Bool) -> Producer a m () -> m Bool
    anyStrict f = P.fold (\acc x -> acc || f x) False id
    inputFiles = readInputFiles options
    reformat = applyFormatter formatter
    writeOutput = Actions.act options

readInputFiles :: Options -> Producer InputFileWithSource IO ()
readInputFiles options =
  determineInputFilePaths (optPaths options) >-> P.mapM readInputFile

determineInputFilePaths :: [FilePath] -> Producer InputFile IO ()
determineInputFilePaths [] = enumeratePath "." >-> P.map InputFilePath
determineInputFilePaths ["-"] = yield InputFromStdIn
determineInputFilePaths paths =
  for (each paths) enumeratePath >-> P.map InputFilePath

readInputFile :: InputFile -> IO InputFileWithSource
readInputFile (InputFilePath path) =
  InputFileWithSource (InputFilePath path) <$> readSource path
readInputFile InputFromStdIn = InputFileWithSource InputFromStdIn <$> readStdin

applyFormatter :: Formatter -> InputFileWithSource -> ReformatResult
applyFormatter (Formatter format) (InputFileWithSource input source) =
  case format source of
    Left error        -> InvalidReformat input error
    Right reformatted -> Reformat input source reformatted

readSource :: HaskellSourceFilePath -> IO HaskellSource
readSource path = HaskellSource <$> readFile path

readStdin :: IO HaskellSource
readStdin = HaskellSource <$> getContents

sourceChangedOrHasSuggestions :: Options -> ReformatResult -> Bool
sourceChangedOrHasSuggestions options (Reformat input source reformatted) =
  case (Options.optAction options) of
    PrintSources -> False
    _ ->
      not (null (suggestions reformatted)) ||
      source /= reformattedSource reformatted

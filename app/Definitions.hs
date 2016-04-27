module Definitions (
    Action(..),
    InputFile(..),
    InputFileWithSource(..),
    ErrorString,
    ReformatResult(..),
    HaskellSourceFilePath,
    HaskellSource(..),
    Options(..),
    ) where

import Language.Haskell.Format
import Language.Haskell.Source.Enumerator (HaskellSourceFilePath)

data Action = PrintDiffs
            | PrintSources
            | PrintFilePaths
            | WriteSources

data Options =
       Options
         { optPrintDiffs     :: Bool
         , optPrintSources   :: Bool
         , optPrintFilePaths :: Bool
         , optWriteSources   :: Bool
         , optPaths          :: [FilePath]
         }

data InputFile = InputFilePath HaskellSourceFilePath
               | InputFromStdIn

instance Show InputFile where
  show (InputFilePath path) = path
  show InputFromStdIn = "-"

data InputFileWithSource = InputFileWithSource InputFile HaskellSource

type ErrorString = String

data ReformatResult = InvalidReformat InputFile ErrorString
                    | Reformat InputFile HaskellSource Reformatted

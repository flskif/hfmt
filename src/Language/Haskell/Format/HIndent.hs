module Language.Haskell.Format.HIndent
  ( autoSettings
  , formatter
  , defaultFormatter
  ) where

import           Control.Applicative
import           Data.ByteString.Builder
import           Data.ByteString.Lazy                as L
import           Data.Maybe
import qualified Data.Text                           as Text
import           Data.Text.Encoding                  as Encoding
import           Data.Text.Lazy                      as TL
import           Data.Text.Lazy.Builder
import qualified Data.Yaml                           as Y
import           HIndent
import           HIndent.Types
import           Language.Haskell.Exts.Extension     (Extension)
import           Path
import qualified Path.Find                           as Path
import qualified Path.IO                             as Path

import           Language.Haskell.Format.Definitions
import           Language.Haskell.Format.Internal

data Settings =
  Settings Config
           (Maybe [Extension])

defaultFormatter :: IO Formatter
defaultFormatter = formatter <$> autoSettings

autoSettings :: IO Settings
autoSettings = do
  config <- getConfig
  return (Settings config Nothing)

-- | Read config from a config file, or return 'defaultConfig'.
getConfig :: IO Config
getConfig = do
  cur <- Path.getCurrentDir
  homeDir <- Path.getHomeDir
  mfile <-
    Path.findFileUp
      cur
      ((== ".hindent.yaml") . toFilePath . filename)
      (Just homeDir)
  case mfile of
    Nothing -> return defaultConfig
    Just file -> do
      result <- Y.decodeFileEither (toFilePath file)
      case result of
        Left e       -> error (show e)
        Right config -> return config

formatter :: Settings -> Formatter
formatter = mkFormatter . hindent

hindent :: Settings -> HaskellSource -> Either String HaskellSource
hindent (Settings config extensions) (HaskellSource source) =
  toHaskellSource <$> reformat config extensions Nothing sourceText
  where
    sourceText = Encoding.encodeUtf8 . Text.pack $ source
    toHaskellSource =
      HaskellSource .
      Text.unpack . Encoding.decodeUtf8 . L.toStrict . toLazyByteString

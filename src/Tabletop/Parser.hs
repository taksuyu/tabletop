module Tabletop.Parser where

import qualified Data.Text as T

import Lens.Micro.Platform
import Options.Applicative
import UnliftIO

data Options = Options
  { _configPath :: Maybe FilePath
  , _sqlitePath :: Maybe T.Text
  } deriving Show

makeLenses ''Options

parseOptions :: MonadUnliftIO m => m Options
parseOptions =
  liftIO . execParser $ info optionsParser fullDesc

optionsParser :: Parser Options
optionsParser =
  Options
  <$> (optional . strOption) configPathOptions
  <*> (optional . strOption) sqlitePathOptions
  <**> helper

configPathOptions :: Mod OptionFields FilePath
configPathOptions =
  long "config-path"
  <> metavar "TARGET"
  <> help "Path to tabletop config. If none is specified then tabletop checks default locations or resorts to defaults."

sqlitePathOptions :: Mod OptionFields T.Text
sqlitePathOptions =
  long "sqlite-path"
  <> metavar "TARGET"
  <> help "Path to Sqlite file database. If none is specified then tabletop reverts to the config location."

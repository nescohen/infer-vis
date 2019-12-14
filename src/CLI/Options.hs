module CLI.Options where

import Options.Applicative

data Options =
    Options { inferOutDir :: Maybe String
            , reportJSON  :: Maybe String
            }
    deriving (Show)

optsParser :: Parser Options
optsParser =
    Options <$> optional (strOption
                 (  long "infer-out"
                 <> metavar "DIRECTORY"
                 <> help "Optionally specify the directory in which infer put its output"
                 ))
            <*> optional (strOption
                 (  long "report-file"
                 <> metavar "FILEPATH"
                 <> help "Optionally directly specify report.json file written by infer"
                 ))

{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module CLI.Report 
    ( resolveReportFile 
    ) where

import CLI.Options (Options(..))
import System.Directory
import System.FilePath

resolveReportFile :: Options -> IO (Either String FilePath)
resolveReportFile Options{..} | Just file <- reportJSON = checkFile file
                              | Just dir <- inferOutDir = checkFile $ dir </> reportFile
                              | otherwise = checkFile defaultReportFile

checkFile :: FilePath -> IO (Either String FilePath)
checkFile p = doesPathExist p >>= \case
                               True  -> return $ Right p
                               False -> return $ Left $ "Error: File not found: " ++ p

defaultReportFile :: FilePath
defaultReportFile = inferOut </> reportFile

inferOut :: FilePath
inferOut = "infer-out"

reportFile :: FilePath
reportFile = "report.json"


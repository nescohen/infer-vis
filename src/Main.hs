{-# LANGUAGE LambdaCase #-}

module Main where

import Brick
import CLI.Options
import CLI.Report
import Data.Aeson
import Interface.Application
import Options.Applicative
import System.Exit
import System.IO

main :: IO ()
main =
  do options <- execParser opts
     resolveReportFile options >>= \case
                 Left err -> do hPutStrLn stderr err
                                exitFailure
                 Right fp -> startWithFile fp
 where 
   opts = info (optsParser <**> helper)
            (  fullDesc
            <> progDesc "Visualize the output of an infer analyze"
            )

   startWithFile fp =
        eitherDecodeFileStrict fp >>= \case
            Left err -> do hPutStrLn stderr $ "Error: " ++ err
                           exitFailure
            Right is -> do _fs <- defaultMain visApp (initialState is)
                           exitSuccess
                           

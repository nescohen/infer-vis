{-# LANGUAGE LambdaCase #-}

module Main where

import Brick (defaultMain)
import CLI.Options (optsParser)
import CLI.Report (resolveReportFile)
import Data.Aeson
import Interface.Application (visApp, initialState)
import Options.Applicative
import System.Exit
import System.IO

main :: IO ()
main =
  do options <- execParser opts
     -- | first need to resolve which report file to read based on given cli options
     --   prefer: specific report -> specific dir -> default based on cwd
     resolveReportFile options >>= \case
                 Left err -> do hPutStrLn stderr err
                                exitFailure
                 Right fp -> startWithFile fp
 where 
   -- | this is an options parser which will automatically provide a help menu and other amenities
   --   w/ the library 'optparse-applicative'
   opts = info (optsParser <**> helper)
            (  fullDesc
            <> progDesc "Visualize the output of an infer analyze"
            )

   -- | decode report file and run Brick app on success
   startWithFile fp =
        eitherDecodeFileStrict fp >>= \case
            Left err -> do hPutStrLn stderr $ "Error: " ++ err
                           exitFailure
            Right is -> do _fs <- defaultMain visApp (initialState is)
                           exitSuccess
                           

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE OverloadedStrings     #-}

module Infer.Types where

import Data.Aeson
import Data.Text (Text)

convertCInt :: Int -> Maybe Int
convertCInt n | n >= 0    = Just n
              | otherwise = Nothing

-- | Represents a general infer issue
data Issue =
    Issue { bugType     :: Text
          , qualifier   :: Text
          , severity    :: Text
          , line        :: Int
          , column      :: Maybe Int
          , file        :: Text
          , bugTrace    :: [BugStep]
          , hash        :: Text
          }
    deriving(Eq,Ord,Show)

instance FromJSON Issue where
    parseJSON = withObject "issue" $ \i ->
     do bugType   <- i .: "bug_type_hum"
        qualifier <- i .: "qualifier"
        severity  <- i .: "severity"
        line      <- i .: "line"
        column    <- convertCInt <$> i .: "column"
        file      <- i .: "file"
        bugTrace  <- i .: "bug_trace"
        hash      <- i .: "hash"
        return Issue{..}

-- | One step in an infer bugtrace
data BugStep =
    BugStep { level       :: Int
            , file        :: Text
            , line        :: Int
            , column      :: Maybe Int
            , description :: Text
            }
    deriving(Eq,Ord,Show)

instance FromJSON BugStep where
    parseJSON = withObject "step" $ \s ->
      do level          <- s .: "level"
         file           <- s .: "filename"
         line           <- s .: "line_number"
         column         <- convertCInt <$> s .: "column_number"
         description    <- s .: "description"
         return BugStep{..}

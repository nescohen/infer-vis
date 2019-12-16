{-# LANGUAGE RecordWildCards #-}

module Interface.Types 
    ( module Infer.Types
    , module Interface.Types
    ) where

import           Brick.Widgets.List
import qualified Data.Vector as V
import           Infer.Types

-- | State for bugtrace view
data BugTraceState =
    BugTraceState { issue :: Issue
                  , steps :: List Name BugStep
                  }

-- | initialize a bugtrace state from an infer issue
mkState :: Issue -> BugTraceState
mkState iss = 
    BugTraceState iss $ list BugTraceList (V.fromList $ bugTrace iss) 1

-- | State for entire UI app
data AppState =
    AppState { issuesList :: List Name Issue
             , selected   :: Maybe BugTraceState
             }

-- | update only the bugtrace state inside entire state - if this gets more complicated, I should use lenses
updateSelected :: (Maybe BugTraceState -> Maybe BugTraceState) -> AppState -> AppState
updateSelected f s@AppState{..} = s { selected = f selected }

-- | Resources type for unique identification
data Name = IssuesList
          | BugTraceList
          | Header
    deriving(Eq,Ord,Show)

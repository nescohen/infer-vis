module Interface.Types 
    ( module Infer.Types
    , module Interface.Types
    ) where

import Brick.Widgets.List
import Infer.Types

data AppState =
    AppState { issuesList :: List Name Issue
             }

data Name = IssuesList
          | Header
    deriving(Eq,Ord,Show)

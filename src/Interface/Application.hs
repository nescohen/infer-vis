{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE RecordWildCards        #-}

module Interface.Application
    ( visApp
    , initialState
    ) where

import           Brick
import           Brick.Widgets.Border
import           Brick.Widgets.List
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import           Interface.Types
import           Graphics.Vty.Input.Events
import           Graphics.Vty.Attributes

initialState :: [Issue] -> AppState
initialState is =
    AppState { issuesList = list IssuesList (V.fromList is) 1
             }

visApp :: App AppState e Name
visApp = App { appDraw          = drawApp 
             , appChooseCursor  = chooseCursor
             , appHandleEvent   = handleEvent
             , appStartEvent    = startEvent
             , appAttrMap       = myAttrMap
             }

drawApp :: AppState -> [Widget Name]
drawApp AppState{..} =
    return $  renderHeader (length (listElements issuesList))
          <=> renderListWithIndex renderListIssue True issuesList

chooseCursor :: AppState -> [CursorLocation Name] -> Maybe (CursorLocation Name)
chooseCursor _ _ = Nothing

handleEvent :: AppState -> BrickEvent Name e -> EventM Name (Next AppState)
handleEvent s (VtyEvent (EvKey (KChar 'q') _ms)) = halt s
handleEvent s (VtyEvent e) =
  do list' <- handleListEventVi handleListEvent e (issuesList s)
     continue $ s { issuesList = list' }
handleEvent s _ = continue s

startEvent :: AppState -> EventM Name AppState
startEvent s = return s

myAttrMap :: AppState -> AttrMap
myAttrMap _s = attrMap defAttr [ ("issue" <> "unselected", defAttr)
                               , ("issue" <> "selected", defAttr `withStyle` standout)
                               ]

welcomeText :: Text
welcomeText =
 T.unlines $ [ "Welcome to the infer visualizer!"
             , "Select an issue from the list below to see the trace."
             , "Menu: "
             , "vi/arrow keys - navigation"
             , "enter - select issue"
             , "q - quit"
             , ""
             ]

issuesHeader :: Int -> Text
issuesHeader n = "Infer reports " <> (T.pack $ show n) <> " total issues."

renderHeader :: Int -> Widget Name
renderHeader n = border $ padRight Max $ txt (welcomeText <> "\n" <> issuesHeader n)

renderListIssue :: Int -> Bool -> Issue -> Widget Name
renderListIssue n True  i =
    withAttr ("issue" <> "selected") $
      txt (issueTitle n i)
renderListIssue n False i =
    withAttr ("issue" <> "unselected") $
      txt (issueTitle n i)

-- | human-readable title for an issue
issueTitle :: Int -> Issue -> Text
issueTitle n Issue{..} =  T.pack (show (n+1)) <> " - "
                       <> bugType <> ": "
                       <> file <> " on line "
                       <> T.pack (show line)

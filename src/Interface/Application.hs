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

visApp :: App AppState e Name
visApp = App 
    -- | drawing function: how to display based on state
    { appDraw          = drawApp 
    -- | cursor selection function: we don't use cursors in this app
    , appChooseCursor  = const . const Nothing
    -- | event handling function: update app state based on input such as keystrokes
    , appHandleEvent   = handleEvent
    -- | event taking place when app starts - does nothing
    , appStartEvent    = \s -> return s
    -- | simple attribute map concerns list selected item only
    , appAttrMap       = myAttrMap
    }

-- | construct initial state based on a list of infer issues
initialState :: [Issue] -> AppState
initialState is =
    AppState { issuesList = list IssuesList (V.fromList is) 1
             , selected   = Nothing
             }

drawApp :: AppState -> [Widget Name]
drawApp AppState{..} =
    return $  renderHeader (length (listElements issuesList))
          <=> case selected of 
                Nothing  -> renderListWithIndex renderListIssue True issuesList
                Just iss -> renderTrace iss

handleEvent :: AppState -> BrickEvent Name e -> EventM Name (Next AppState)
handleEvent s (VtyEvent (EvKey (KChar 'q') _ms)) = halt s -- always quit when q is pressed
handleEvent s (VtyEvent (EvKey (KChar 'c') [MCtrl])) = halt s -- respect ctrl-c
handleEvent s@AppState{..} (VtyEvent e) | Just bugTrace <- selected = -- handle differently if issue is selected
  case e of
    EvKey KBS _ms -> continue $ s { selected = Nothing }
    _ -> do list' <- handleListEventVi handleListEvent e (steps bugTrace)
            continue $ updateSelected (updateBugStateList list') s
                                        | otherwise = -- default interface which is list of issues
  case e of
    EvKey KEnter _ms -> continue $ selectIssue s
    _ -> do list' <- handleListEventVi handleListEvent e issuesList
            continue $ s { issuesList = list' }
 where
  updateBugStateList l (Just t) = Just (t { steps = l })
  updateBugStateList _ Nothing  = Nothing
handleEvent s _ = continue s

selectIssue :: AppState -> AppState
selectIssue s@AppState{..} | Just (_, iss) <- listSelectedElement issuesList = s { selected = Just (mkState iss) }
                           | otherwise = s

myAttrMap :: AppState -> AttrMap
myAttrMap _s = attrMap defAttr [ ("issue" <> "unselected", defAttr)
                               , ("issue" <> "selected", defAttr `withStyle` standout)
                               ]

welcomeText :: Text
welcomeText =
 T.unlines $ [ "Welcome to the infer visualizer!"
             , "Select an issue from the list below to see the trace."
             , ""
             , "Menu: "
             , "  vi/arrow keys - navigation"
             , "  enter - select"
             , "  backspace - go back"
             , "  q - quit"
             , ""
             ]

renderHeader :: Int -> Widget Name
renderHeader n = border $ padRight Max $ txt (welcomeText <> "\n" <> issuesHeader n)
 where
  issuesHeader num = "Infer reports " <> (T.pack $ show num) <> " total issues."

renderListIssue :: Int -> Bool -> Issue -> Widget Name
renderListIssue n True i =
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

renderTrace :: BugTraceState -> Widget Name
renderTrace BugTraceState{..} = renderListWithIndex renderStep True steps

renderStep :: Int -> Bool -> BugStep -> Widget Name
renderStep index selected BugStep{..} =
     padTop (Pad 1) $ withAttr att
      (  txt ((T.pack $ show index) <> ".")
     <=> txt (file <> " at line " <> (T.pack $ show line) <> ".")
     <=> txt description
      )
 where
   att = if selected then ("issue" <> "selected")
         else ("issue" <> "unselected")

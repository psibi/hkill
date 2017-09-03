{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Brick
import qualified Brick.AttrMap as A
import qualified Brick.Focus as F
import qualified Brick.Main as M
import qualified Brick.Types as T
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Core
import qualified Brick.Widgets.Edit as E
import Brick.Widgets.List
import Control.Monad.IO.Class
import Data.Foldable (fold)
import Data.List (isPrefixOf)
import Data.Monoid
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V
import Lens.Micro
import Lens.Micro.TH
import System.Linux.Process
import Data.Text (Text, pack)
import qualified Data.Text as Text
import Data.Either (isRight)

searchWidget :: [String] -> Widget SearchWidget
searchWidget xs = (str $ unlines xs)

keyBindingWidget :: Widget SearchWidget
keyBindingWidget = str ("Key Bindings" <> newline <> newline <> fold bindings)

bindings :: [String]
bindings =
  [ "Esc : Quit the application"
  , newline
  , "Ctrl-k : Send SIGINT"
  , newline
  , "Ctrl-i : Process information"
  , newline
  , "r : Refresh"
  ]

newline :: String
newline = "\n"

logWidget :: [String] -> Widget SearchWidget
logWidget logs =
  viewport Logger Vertical $ str ("LOG MESSAGES" <> newline <> fold logs)

loggerScroll :: M.ViewportScroll SearchWidget
loggerScroll = M.viewportScroll Logger

data SearchWidget
  = SearchEdit
  | SearchResult
  | Logger
  deriving (Ord, Show, Eq)

data KillState = KillState
  { _focusRing :: F.FocusRing SearchWidget
  , _searchEdit :: E.Editor String SearchWidget
  , _searchResult :: List SearchWidget Text
  , _logMessages :: [String]
  , _systemProcesses :: [Either String ProcessInfo]
  }

makeLenses ''KillState

initialState :: [Either String ProcessInfo] -> KillState
initialState xs =
  KillState
  { _focusRing = F.focusRing [SearchEdit, SearchResult, Logger]
  , _searchEdit = E.editor SearchEdit (Just 1) ""
  , _searchResult = killResultsView $ processesToNames xs
  , _logMessages = []
  , _systemProcesses = xs
  }

listDrawElement :: Bool -> Text -> Widget SearchWidget
listDrawElement sel a =
  let selStr s =
        if sel
          then withAttr "foundFull" $ txt s
          else txt s
  in selStr a

renderProcessInfo :: ProcessInfo -> [String]
renderProcessInfo ProcessInfo {..} =
  [ newline
  , Text.unpack procName
  , newline
  , "PID " <> (show $ pid procPid)
  , newline
  , "PPID " <> (show $ pid procPpid)
  , newline
  , "Memory size " <> Text.unpack procVmSize
  , newline
  , "Threads: " <> show procThreads
  , newline
  ]

drawUI :: KillState -> [Widget SearchWidget]
drawUI st = [ui]
  where
    edit1 :: Widget SearchWidget
    edit1 =
      F.withFocusRing
        (st ^. focusRing)
        (E.renderEditor searchWidget)
        (st ^. searchEdit)
    resultView :: Widget SearchWidget
    resultView =
      F.withFocusRing
        (st ^. focusRing)
        (renderList listDrawElement)
        (st ^. searchResult)
    ui =
      borderWithLabel (str "hkill") $
      vBox
        [ (((str "Search ") <+> edit1) <=> hBorder <=> resultView) <+>
          vBorder <+>
          (keyBindingWidget <=> hBorder <=> logWidget (st ^. logMessages))
        ]

killResultsView :: [Text] -> List SearchWidget Text
killResultsView xs = list SearchResult (Vec.fromList xs) 1

appCursor
  :: KillState
  -> [T.CursorLocation SearchWidget]
  -> Maybe (T.CursorLocation SearchWidget)
appCursor = F.focusRingCursor (^. focusRing)

appEvent
  :: KillState
  -> T.BrickEvent SearchWidget e
  -> T.EventM SearchWidget (T.Next KillState)
appEvent st (T.VtyEvent ev) =
  case ev of
    V.EvKey V.KEsc [] -> M.halt st
    V.EvKey (V.KChar '\t') [] -> M.continue $ st & focusRing %~ F.focusNext
    V.EvKey (V.KChar 'n') [V.MCtrl] -> M.continue $ st & searchResult %~ listMoveDown
    V.EvKey (V.KChar 'p') [V.MCtrl] -> M.continue $ st & searchResult %~ listMoveUp
    V.EvKey (V.KChar 'j') [V.MMeta] -> do
      let (currentProcess :: Maybe Pid) =
            case listSelectedElement (st ^. searchResult) of
              Nothing -> Nothing
              Just (_, pname) -> either (const Nothing) Just (selectedPid pname)
      case currentProcess of
        Nothing -> M.continue st
        Just pid -> do
          let procs = filterPid pid (st ^. systemProcesses)
          case procs of
            [] -> M.continue st
            ((Right process):_) -> do
              M.vScrollToEnd loggerScroll
              M.continue $ st & logMessages %~ (++ renderProcessInfo process)
            _ -> M.continue st
    V.EvKey V.KBackTab [] -> M.continue $ st & focusRing %~ F.focusPrev
    _ ->
      case F.focusGetCurrent (st ^. focusRing) of
        Just SearchEdit -> do
          T.handleEventLensed st searchEdit E.handleEditorEvent ev >>=
            \st' -> do
              let searchStr :: Text =
                    Text.pack $ fold $ E.getEditContents $ _searchEdit st'
                  filterProcesses =
                    if searchStr == ""
                      then processesToNames $ st ^. systemProcesses
                      else filter
                             (\x -> searchStr `Text.isPrefixOf` x)
                             (processesToNames $ st ^. systemProcesses)
                  newSt =
                    st'
                    { _searchResult = killResultsView filterProcesses
                    }
              M.continue newSt
        Just SearchResult -> do
          M.continue =<< T.handleEventLensed st searchResult handleListEvent ev
        Just Logger ->
          case ev of
            V.EvKey V.KUp [] ->
              M.continue =<< (M.vScrollBy loggerScroll (-1) >> return st)
            V.EvKey V.KDown [] ->
              M.continue =<< (M.vScrollBy loggerScroll 1 >> return st)
            _ -> M.continue =<< return st
        Nothing -> M.continue =<< return st
appEvent st _ = M.continue st

killApp :: App KillState e SearchWidget
killApp =
  App
  { appDraw = drawUI
  , appChooseCursor = appCursor
  , appHandleEvent = appEvent
  , M.appStartEvent = return
  , M.appAttrMap = const theMap
  }

theMap :: A.AttrMap
theMap =
  A.attrMap
    V.defAttr
    [ (E.editAttr, V.white `on` V.blue)
    , (E.editFocusedAttr, V.black `on` V.yellow)
    , (listAttr, fg V.cyan)
    , (listSelectedFocusedAttr, V.black `on` V.yellow)
    , ("foundFull", V.black `on` V.yellow)
    ]

renderProcess :: ProcessInfo -> Text
renderProcess ps =
  procName ps <> " (" <> (pack $ show $ pid $ procPid ps) <> ")"

processesToNames :: [Either String ProcessInfo] -> [Text]
processesToNames procs =
  filter (\x -> x /= "") $ map (either (const "") renderProcess) procs

main :: IO ()
main = do
  procs <- getAllProcessInfoDS
  defaultMain killApp $ initialState procs
  return ()
-- main :: IO ()
-- main = do
--   let x = selectedPid "polkitd (1043)"
--   print x

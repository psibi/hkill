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
import Lib

searchWidget :: [String] -> Widget SearchWidget
searchWidget xs = (str $ unlines xs)

keyBindingWidget :: Widget SearchWidget
keyBindingWidget = str ("Key Bindings" <> newline <> newline <> fold bindings)

bindings :: [String]
bindings =
  [ "q : Quit the application"
  , newline
  , "k : Send SIGINT"
  , newline
  , "i : Process information"
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
  , _searchResult :: List SearchWidget String
  , _logMessages :: [String]
  , _appProcesses :: [String]
  }

makeLenses ''KillState

initialState :: KillState
initialState =
  KillState
  { _focusRing = F.focusRing [SearchEdit, SearchResult, Logger]
  , _searchEdit = E.editor SearchEdit (Just 1) ""
  , _searchResult = killResultsView sampleProcesses
  , _logMessages = []
  , _appProcesses = sampleProcesses
  }

listDrawElement :: Bool -> String -> Widget SearchWidget
listDrawElement sel a =
  let selStr s =
        if sel
          then str $ ("* " <> s)
          else str s
  in selStr a

sampleLog :: [String]
sampleLog =
  [newline, "Process emacs", newline, "PID 3", newline, "Memory 30 MB"]

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

sampleProcesses :: [String]
sampleProcesses = ["emacs", "joe", "systemd"]

killResultsView :: [String] -> List SearchWidget String
killResultsView xs = list SearchResult (Vec.fromList xs) 1

appCursor ::
     KillState
  -> [T.CursorLocation SearchWidget]
  -> Maybe (T.CursorLocation SearchWidget)
appCursor = F.focusRingCursor (^. focusRing)

handleLoggerViewEVent ::
     V.Event -> Widget SearchWidget -> T.EventM SearchWidget (T.Next KillState)
handleLoggerViewEVent = error "nopey"

appEvent ::
     KillState
  -> T.BrickEvent SearchWidget e
  -> T.EventM SearchWidget (T.Next KillState)
appEvent st (T.VtyEvent ev) =
  case ev of
    V.EvKey V.KEsc [] -> M.halt st
    V.EvKey (V.KChar '\t') [] -> M.continue $ st & focusRing %~ F.focusNext
    V.EvKey (V.KChar 'i') [] -> M.continue $ st & logMessages %~ (++ sampleLog)
    V.EvKey V.KBackTab [] -> M.continue $ st & focusRing %~ F.focusPrev
    _ ->
      case F.focusGetCurrent (st ^. focusRing) of
        Just SearchEdit -> do
          T.handleEventLensed st searchEdit E.handleEditorEvent ev >>= \st' -> do
            let searchStr :: String = fold $ E.getEditContents $ _searchEdit st'
                filterProcesses =
                  if searchStr == []
                    then sampleProcesses
                    else filter (\x -> searchStr `isPrefixOf` x) sampleProcesses
                newSt = st' {_searchResult = killResultsView filterProcesses}
            M.continue newSt
        Just SearchResult -> do
          let searchStr :: String = fold $ E.getEditContents $ _searchEdit st
              filterProcesses =
                if searchStr == []
                  then sampleProcesses
                  else filter (\x -> searchStr `isPrefixOf` x) sampleProcesses
              newSt = st {_searchResult = killResultsView filterProcesses}
          M.continue =<<
            T.handleEventLensed newSt searchResult handleListEvent ev
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
    ]

main :: IO ()
main = defaultMain killApp initialState >> return ()

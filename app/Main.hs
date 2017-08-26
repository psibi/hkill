{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Lib
import Lens.Micro
import Lens.Micro.TH
import Brick
import Brick.Widgets.Center
import Brick.Widgets.Core
import Brick.Widgets.Border
import Data.Foldable (fold)
import Data.Monoid
import Brick.Widgets.List
import qualified Brick.Focus as F
import qualified Brick.Types as T
import qualified Brick.AttrMap as A
import qualified Brick.Widgets.Edit as E
import qualified Graphics.Vty as V
import qualified Brick.Main as M
import qualified Data.Vector as Vec

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

logWidget :: Widget SearchWidget
logWidget = (str ("LOG MESSAGES \n\nTest 1 msg"))

data SearchWidget
  = SearchEdit
  | SearchResult
  deriving (Ord, Show, Eq)

data KillState = KillState
  { _focusRing :: F.FocusRing SearchWidget
  , _searchEdit :: E.Editor String SearchWidget
  , _searchResult :: List SearchWidget String
  }

makeLenses ''KillState

initialState :: KillState
initialState =
  KillState
  { _focusRing = F.focusRing [SearchEdit, SearchResult]
  , _searchEdit = E.editor SearchEdit (Just 1) ""
  , _searchResult = killResultsView
  }

listDrawElement :: Bool -> String -> Widget SearchWidget
listDrawElement sel a = 
    let selStr s = if sel
                   then (str $ "<" <> a <> ">")
                   else str s
    in selStr a


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
          vBorder <+> (keyBindingWidget <=> hBorder <=> logWidget)
        ]

killResultsView :: List SearchWidget String
killResultsView = list SearchResult (Vec.fromList ["emacs", "joe", "systemd"]) 1

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
    V.EvKey V.KBackTab [] -> M.continue $ st & focusRing %~ F.focusPrev
    _ ->
      M.continue =<<
      case F.focusGetCurrent (st ^. focusRing) of
        Just SearchEdit ->
          T.handleEventLensed st searchEdit E.handleEditorEvent ev
        Just SearchResult ->
          T.handleEventLensed st searchResult handleListEvent ev
        Nothing -> return st
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
    ]

main :: IO ()
main = defaultMain killApp initialState >> return ()

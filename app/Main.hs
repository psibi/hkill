module Main where

import Lib
import Brick
import Brick.Widgets.Center
import Brick.Widgets.Core
import Brick.Widgets.Border
import Data.Foldable (fold)
import Data.Monoid

searchWidget :: Widget ()
searchWidget = padRight Max $ (str ("Search"))

keyBindingWidget :: Widget ()
keyBindingWidget = str ("Key Bindings" <> newline <> newline <> fold bindings)

bindings :: [String]
bindings =
  [ "q : Quit the application"
  , newline
  , "k : Send SIGINT"
  , newline
  , "i : Process information"
  ]

newline :: String
newline = "\n"

logWidget :: Widget ()
logWidget = (str ("LOG MESSAGES \n\nTest 1 msg"))

ui :: Widget ()
ui =
  borderWithLabel (str "hkill") $
  vBox
    [searchWidget <+> vBorder <+> (keyBindingWidget <=> hBorder <=> logWidget)]

main :: IO ()
main = simpleMain ui

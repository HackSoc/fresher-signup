{-# LANGUAGE OverloadedStrings #-}

module Main where

import Brick
import Brick.AttrMap
import Brick.Types
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Edit
import Brick.Widgets.Dialog
import Control.Monad.IO.Class
import Data.Functor
import Graphics.Vty
import Graphics.Vty.Attributes

data Selected = Email | Paid | Submit deriving Eq

type State = (Editor, Bool, Selected)

defaultState :: State
defaultState = (defaultEditor, False, Email) where
  defaultEditor = editor "email" (str . unlines) (Just 1) "@york.ac.uk"

outFile :: FilePath
outFile = "emails.txt"

outFilePaid :: FilePath
outFilePaid = "emails-paid.txt"

draw :: State -> Widget
draw (ed, p, s) = center $ hCenter (hLimit 47 hacksoc) <=> hCenter (pad email <=> paid <=> pad button) where
  hacksoc = vBox
    [ str "╦   ╦          ╦   ╔══╣"
    , str "║   ║          ║   ║"
    , str "╠═══╣ ╔═╗ ╔══╗ ║ ╔ ╚══╗ ╔══╗ ╔══╗"
    , str "║   ║ ╔═╣ ║    ╠═╣    ║ ║  ║ ║"
    , str "╩   ╩ ╚═╩ ╚══╝ ╩ ╚ ╠══╝ ╚══╝ ╚══╝"
    , str "     the " <+> withAttr "cs" (str "computer science") <+> str " society"
    ]

  email = pad (select (s == Email) $ str "Email address:") <+> ((str " " <+>) . border . hLimit 30 . vLimit 1 $ renderEditor ed)

  paid = (select (s == Paid) $ str "Paid:") <+> str (if p then " [X]" else " [ ]")

  button = select (s == Submit) $ str "[submit]"

  pad = translateBy $ Location (0, 1)

  select True  = withAttr "select"
  select False = id

handle :: State -> Event -> EventM (Next State)
handle _ (EvKey KEsc []) = continue defaultState
handle st (EvKey (KChar 'c') [MCtrl]) = halt st

handle (ed, p, Email)  (EvKey (KChar '\t') []) = continue (ed, p, Paid)
handle (ed, p, Paid)   (EvKey (KChar '\t') []) = continue (ed, p, Submit)
handle (ed, p, Submit) (EvKey (KChar '\t') []) = continue (ed, p, Email)

handle (ed, p, Email)  (EvKey KDown []) = continue (ed, p, Paid)
handle (ed, p, Paid)   (EvKey KDown []) = continue (ed, p, Submit)
handle (ed, p, Submit) (EvKey KDown []) = continue (ed, p, Email)

handle (ed, p, Email)  (EvKey KUp []) = continue (ed, p, Submit)
handle (ed, p, Paid)   (EvKey KUp []) = continue (ed, p, Email)
handle (ed, p, Submit) (EvKey KUp []) = continue (ed, p, Paid)

handle (ed, p, Email) e = handleEvent e ed >>= \ed' -> continue (ed', p, Email)
handle (ed, p, Paid) (EvKey KEnter []) = continue (ed, not p, Paid)
handle (ed, p, Paid) (EvKey (KChar ' ') []) = continue (ed, not p, Paid)
handle (ed, p, Submit) (EvKey KEnter []) = saveEmail >> continue defaultState where
  saveEmail
    | p         = liftIO . appendFile outFilePaid . unlines $ getEditContents ed
    | otherwise = liftIO . appendFile outFile     . unlines $ getEditContents ed

handle s _ = continue s

main :: IO ()
main = void $ defaultMain app defaultState where
  app = App
    { appDraw         = (:[]) . draw
    , appChooseCursor = \_ _ -> Nothing
    , appHandleEvent  = handle
    , appStartEvent   = return
    , appAttrMap      = const $ attrMap (fg green)
      [ ("cs", fg brightWhite)
      , ("edit", fg white)
      , ("select", withStyle (withStyle (fg green) bold) underline)]
    , appLiftVtyEvent = id
    }

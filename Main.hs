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
handle st@(ed, paid, selected) ev = case ev of
  -- 'esc' resets
  EvKey KEsc [] -> continue defaultState

  -- C-c terminates
  EvKey (KChar 'c') [MCtrl] -> halt st

  -- 'tab', 'up', and 'down' navigate
  EvKey (KChar '\t') [] -> continue (ed, paid, nextsel)
  EvKey KDown [] -> continue (ed, paid, nextsel)
  EvKey KUp   [] -> continue (ed, paid, priorsel)

  -- contextual controls
  _ -> case (selected, ev) of
    -- editor can be typed into
    (Email, _) -> handleEvent ev ed >>= \ed' -> continue (ed', paid, selected)

    -- checkbox can be toggled with 'enter' or 'space'
    (Paid, EvKey KEnter [])      -> continue (ed, not paid, selected)
    (Paid, EvKey (KChar ' ') []) -> continue (ed, not paid, selected)

    -- button can be pressed with 'enter'
    (Submit, EvKey KEnter []) -> saveEmail >> continue defaultState

    -- fall through
    _ -> continue st

  where
    nextsel = case selected of
      Email  -> Paid
      Paid   -> Submit
      Submit -> Email

    priorsel = case selected of
      Email  -> Submit
      Paid   -> Email
      Submit -> Paid

    saveEmail = liftIO . appendFile fname . unlines $ getEditContents ed where
      fname = if paid then outFilePaid else outFile

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

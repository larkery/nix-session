import XMonad hiding ( (|||) )
import XMonad.Layout.Tabbed
import XMonad.Layout.NoBorders
import XMonad.Layout.LayoutCombinators
import XMonad.Util.Themes
import XMonad.Config.Desktop
import System.Taffybar.Hooks.PagerHints (pagerHints)
import qualified XMonad.Layout.Renamed as Ren
import qualified XMonad.Layout.MouseResizableTile as MRT
import qualified XMonad.Actions.DwmPromote as DWM
import qualified XMonad.Actions.DynamicWorkspaces as DW

import qualified XMonad.StackSet as W

import qualified XMonad.Prompt as XP
import qualified XMonad.Prompt.Window as XPW
import qualified XMonad.Actions.CycleRecentWS as Recent
import qualified XMonad.Layout.Hidden as H

import XMonad.Util.EZConfig
import Data.List (isInfixOf, (\\))
import Data.Char (toLower)

layout = XMonad.Layout.NoBorders.smartBorders $
         Ren.renamed [Ren.CutWordsLeft 1] $
         H.hiddenWindows $
         (tiled ||| htiled ||| full)
  where
    tbase = MRT.mouseResizableTile {
      MRT.masterFrac = 0.5,
      MRT.fracIncrement = 0.05,
      MRT.draggerType = MRT.FixedDragger
        { MRT.gapWidth = 4, MRT.draggerWidth = 6 }
      }
    tiled = Ren.renamed [Ren.Replace "s"] $ tbase
    htiled = Ren.renamed [Ren.Replace "d"] $ tbase { MRT.isMirrored = True }
    full = Ren.renamed [Ren.Replace "f"] $ XMonad.Layout.NoBorders.noBorders Full

prompt = XP.defaultXPConfig
   { XP.font = "xft:Sans:pixelsize=16"
   , XP.height = 32
   , XP.position = XP.Top
   , XP.borderColor = "#404040"
   , XP.fgColor = "#F5f5f5"
   , XP.bgColor = "#494949"
   , XP.fgHLight = "#F5f5f5"
   , XP.bgHLight = "#4169e1"
   , XP.promptBorderWidth = 4
   , XP.searchPredicate = isInfixOf . (map toLower)
 }

main = xmonad $
  pagerHints $
  desktopConfig
    { modMask     = mod4Mask
    , layoutHook = desktopLayoutModifiers $
                   layout
    , workspaces = map show [1..9]
    }
    `removeKeysP`
    [p ++ [n] | p <- ["M-", "M-S-"], n <- ['1'..'9']]
    `additionalKeysP`
    ([
      -- not sure
      ("M-<Return>", DWM.dwmpromote),

      ("M-a e", spawn "emacsclient -c -n"),
      ("M-a w", spawn "vimb"),

      ("M-h", XPW.windowPromptBring prompt),
      ("M-j", XPW.windowPromptGoto prompt),
      ("M-k", kill),
      ("M-l", Recent.cycleRecentWS [xK_Super_L] xK_l xK_j),

      ("M-m",   withFocused H.hideWindow),
      ("M-S-m", H.popNewestHiddenWindow),
      ("M-a M-a", spawn "dmenu_run"),

      ("M-u", sendMessage Shrink),
      ("M-p", sendMessage Expand),
      ("M-i", sendMessage MRT.ExpandSlave),
      ("M-o", sendMessage MRT.ShrinkSlave)
     ]
     ++
     [ ("M-" ++ k, sendMessage $ JumpToLayout k) | k <- ["s","d","f"] ]
     ++
     [(prefix ++ (show number), (action (number - 1))) |
      (prefix, action) <- [("M-", DW.withNthWorkspace W.greedyView),
                           ("M-S-", DW.withNthWorkspace W.shift)],
       number <- [1..9]]
    )

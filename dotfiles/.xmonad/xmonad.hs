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

import XMonad.Util.EZConfig
import Data.List (isInfixOf, (\\))
import Data.Char (toLower)

layout = XMonad.Layout.NoBorders.smartBorders $
         (tiled ||| htiled ||| full)
  where
     -- tiled = Tall 1 (3/100) (1/2)
     tiled = Ren.renamed [Ren.Replace "T"] $
             MRT.mouseResizableTile {
             MRT.masterFrac = 0.5,
             MRT.fracIncrement = 0.05,
             MRT.draggerType = MRT.FixedDragger { MRT.gapWidth = 4, MRT.draggerWidth = 6 }
     }
     htiled = Ren.renamed [Ren.Replace "Y"] $
             MRT.mouseResizableTile {
             MRT.masterFrac = 0.5,
             MRT.fracIncrement = 0.05,
             MRT.draggerType = MRT.FixedDragger { MRT.gapWidth = 4, MRT.draggerWidth = 6},
             MRT.isMirrored = True
     }
     full = Ren.renamed [Ren.Replace "F"] $ XMonad.Layout.NoBorders.noBorders Full

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

      ("M-g", XPW.windowPromptGoto prompt)

    ] ++
    [(prefix ++ (show number), (action (number - 1))) |
      (prefix, action) <- [("M-", DW.withNthWorkspace W.greedyView),
                           ("M-S-", DW.withNthWorkspace W.shift)],
      number <- [1..9]]
    )

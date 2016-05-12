import XMonad hiding ( (|||) )
import XMonad.Layout.Tabbed
import XMonad.Layout.NoBorders
import XMonad.Layout.LayoutCombinators

import qualified XMonad.Layout.TwoUp as TU

import XMonad.Util.Themes
import XMonad.Config.Desktop
import System.Taffybar.Hooks.PagerHints (pagerHints)
import qualified XMonad.Layout.Renamed as Ren
import qualified XMonad.Layout.MouseResizableTile as MRT
import qualified XMonad.Actions.DwmPromote as DWM
import XMonad.Actions.FindEmptyWorkspace
import qualified XMonad.Actions.DynamicWorkspaces as DW

import qualified XMonad.StackSet as W

import qualified XMonad.Prompt as XP
import qualified XMonad.Prompt.Window as XPW
import qualified XMonad.Actions.CycleRecentWS as Recent
import qualified XMonad.Layout.BoringWindows as Boring
import XMonad.Actions.RotSlaves
import qualified XMonad.Layout.LimitWindows as Lim
import XMonad.Actions.WindowBringer (bringWindow) 

import XMonad.Util.EZConfig
import Data.List (isInfixOf, (\\))
import Data.Char (toLower)
import Data.Maybe (isJust, fromJust, listToMaybe)

import qualified XMonad.Layout.MouseResizableColumns as MRC

layout = XMonad.Layout.NoBorders.smartBorders $
         Boring.boringAuto $
--         Ren.renamed [Ren.CutWordsLeft 1] $
         TU.twoUp $
--         Lim.limitWindows 10000 $
         (tiled ||| htiled ||| full -- ||| MRC.mrc
         )
  where
    tbase = MRT.mouseResizableTile {
      MRT.masterFrac = 0.5,
      MRT.fracIncrement = 0.05,
      MRT.draggerType = MRT.FixedDragger
        { MRT.gapWidth = 3, MRT.draggerWidth = 6 }
      }
    tiled = Ren.renamed [Ren.Replace "s"] $ tbase
    htiled = Ren.renamed [Ren.Replace "d"] $ tbase { MRT.isMirrored = True }
    full = Ren.renamed [Ren.Replace "f"] $ XMonad.Layout.NoBorders.noBorders Full

rotate [] = []
rotate (x:xs) = xs ++ [x]

cycleRecentPopulated = Recent.cycleWindowSets candidates
  where candidates w = map (W.view `flip` w) (recentTags w)
        recentTags w = map W.tag $ rotate $ hiddenWithWindows w
        hiddenWithWindows w = filter (isJust . W.stack) $ W.workspaces w

bringFromMin = windows bringHeadOfMin
  where bringHeadOfMin :: WindowSet -> WindowSet
        bringHeadOfMin ws = 
          let minSpaces = filter ((== "min") . W.tag) $ W.workspaces ws
              firstMin :: Maybe Window
              firstMin =
                do thews <- listToMaybe minSpaces
                   let wins = W.integrate' $ W.stack $ thews
                   win <- listToMaybe wins
                   return $ win
          in if isJust firstMin then bringWindow (fromJust firstMin) ws
             else ws

main = xmonad $
  pagerHints $
  desktopConfig
    { modMask     = mod4Mask
    , layoutHook = desktopLayoutModifiers $
                   layout
    , workspaces = (map show [1..9]) ++ ["min"]
    , focusedBorderColor = "#00bfff"
    , borderWidth = 2
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
      ("M-j", XPW.windowPromptGoto  prompt),
      ("M-k", kill),
      ("M-l", cycleRecentPopulated [xK_Super_L] xK_l xK_j),
      ("M-y", viewEmptyWorkspace),

      ("M-<Tab>", Boring.focusDown),
      ("M-S-<Tab>", Boring.focusUp),
      
      ("M-m", windows $ W.shift "min"),
       ("M-S-m", bringFromMin),

       ("M-a M-a", spawn "dmenu_run"),

       ("M-u", sendMessage Shrink),
       ("M-p", sendMessage Expand),

       ("M-i", Boring.focusUp),
       ("M-o", Boring.focusDown),
       ("M-S-i", windows W.swapUp),
       ("M-S-o", windows W.swapDown),
      
       ("M-[", rotSlavesUp),
       ("M-]", rotSlavesDown),
       
       ("M-v",   Lim.setLimit 10000),
       ("M-S-v", Lim.setLimit 2)
     ]
     ++
     [ ("M-" ++ k, sendMessage $ JumpToLayout k) | k <- ["s","d","f"] ]
     ++
     [(prefix ++ (show number), (action (number - 1))) |
      (prefix, action) <- [("M-",   DW.withNthWorkspace W.greedyView),
                           ("M-S-", DW.withNthWorkspace W.shift)],
       number <- [1..9]]
    )


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

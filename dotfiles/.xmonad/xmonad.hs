{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

import XMonad hiding ( (|||) )
import XMonad.Layout.Tabbed
import XMonad.Layout.NoBorders
import XMonad.Layout.LayoutCombinators
import System.Exit
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
import qualified XMonad.Prompt.Shell as XPS
import qualified XMonad.Prompt.Window as XPW

import qualified XMonad.Actions.CycleRecentWS as Recent
import qualified XMonad.Layout.BoringWindows as Boring

import XMonad.Actions.RotSlaves

import XMonad.Actions.WindowBringer (bringWindow) 

import XMonad.Util.EZConfig
import Data.List (isInfixOf, (\\))
import Data.Char (toLower)
import Data.Maybe (isJust, fromJust, listToMaybe)

import Control.Monad (when)

import qualified XMonad.Layout.LimitWindows as Limit

import qualified XMonad.Layout.VarialColumn as VC

import XMonad.Layout.LayoutModifier

import XMonad.Actions.ShowText (flashText, defaultSTConfig)
import XMonad.Layout.ShowWName

import Data.Maybe (fromMaybe)
import qualified XMonad.Actions.CycleWS as C

as n x = Ren.renamed [Ren.Replace n] x

data AddCount a = AddCount Int Int deriving (Show, Read)

instance LayoutModifier AddCount a where
  pureModifier s r stackm wrs =
    let total = length $ W.integrate' stackm
        visible = length wrs
        state = (AddCount total visible) in
      (wrs, Just state)
  modifyDescription (AddCount t v) l
    | t > v = description l ++ " (+" ++ (show (t - v)) ++ ")"
    | otherwise = description l

addCount = ModifiedLayout ((AddCount 0 0) :: AddCount Window)

layout = -- showWName' defaultSWNConfig 
         -- {
         --   swn_font = "xft:Sans:pixelsize=32"
         -- , swn_bgcolor = "orange"
         -- , swn_fade = 0.5
         -- } $
         XMonad.Layout.NoBorders.smartBorders $
         addCount $
         Boring.boringAuto $ (tiled ||| twocol ||| full)
  where
    tiled = as "s" $ VC.varial
    twocol = as "d" $ Limit.limitWindows 2 $ VC.varial
    full = as "f" $ Full

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

-- bindings which work in certain layouts

inLayout :: [(String, X ())] -> X () -> X ()
inLayout as d =
  do lname <- gets (description . W.layout . W.workspace . W.current . windowset)
     let lname' = head $ words lname
     fromMaybe d $ lookup lname' as 

focusUp = inLayout
  [("f", windows W.focusUp)]
  Boring.focusUp

focusDown = inLayout
  [("f", windows W.focusDown)]
  Boring.focusDown

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
    ([("M-<Escape>", spawn "xmonad --recompile; xmonad --restart"),
      ("M-S-<Escape>", io (exitWith ExitSuccess)),
      ("M-<Return>", DWM.dwmpromote),
      
      ("M-l", C.moveTo C.Prev C.NonEmptyWS),
      ("M-;", C.moveTo C.Next C.NonEmptyWS),
      ("M-y", tagToEmptyWorkspace),

      ("M-a e", spawn "emacsclient -c -n"),
      ("M-a w", spawn "vimb"),
      ("M-a d", spawn "dmenu_run"),
      ("M-a M-a", XPS.shellPrompt prompt),
      ("M-a h", spawn "systemctl hibernate"),
      ("M-a l", spawn "systemctl suspend"),
      ("M-a m", spawn "xdg-open mailto:"),
      ("M-a M-m", spawn "notmuch new"),
      ("M-a s", spawn "xterm"),

      ("M-h", XPW.windowPromptBring prompt),
      ("M-j", XPW.windowPromptGoto  prompt),
      ("M-k", kill),

      ("M-<Tab>", focusDown),
      ("M-S-<Tab>", focusUp),
      
      ("M-m", windows $ W.shift "min"),
      ("M-S-m", bringFromMin),

      ("M-p", focusUp),
      --("M-o", focusDown),
      ("M-n", focusDown),
      
      ("M-,", rotSlavesUp),
      ("M-.", rotSlavesDown),
      
      ("M-S-n", withFocused $ \w -> sendMessage $ VC.DownOrRight w),
      ("M-S-p", withFocused $ \w -> sendMessage $ VC.UpOrLeft w),
      
      ("M-c M-c",   withFocused $ \w -> sendMessage $ VC.ToNewColumn w),      
      ("M-c c", withFocused $ \w -> sendMessage $ VC.GrabColumn w),
      ("M-c h", withFocused $ \w -> sendMessage $ VC.GrabRow w),
      ("M-c e", withFocused $ \w -> sendMessage $ VC.EqualizeColumn 1 w),
      ("M-c q", withFocused $ \w -> sendMessage $ VC.EqualizeColumn 0.5 w),

      ("M-i", withFocused $ \w -> sendMessage $ VC.Embiggen 0.1 w),
      ("M-u", withFocused $ \w -> sendMessage $ VC.Embiggen (-0.1 :: Rational) w)
     ]
     ++
     [ ("M-" ++ k, ((sendMessage $ JumpToLayout k))) | k <- ["s","d","f"] ]
     ++
     [(prefix ++ (show number), (action (number - 1))) |
      (prefix, action) <- [("M-",   DW.withNthWorkspace W.greedyView),
                           ("M-S-", DW.withNthWorkspace W.shift)],
       number <- [1..9]]
      ++
     [(prefix ++ key, (action number)) |
      (prefix, action) <- [("M-",   DW.withNthWorkspace W.greedyView),
                           ("M-S-", DW.withNthWorkspace W.shift)],
       (number, key) <- zip [0..] ["q","w","e","r"]]
    )


prompt = XP.defaultXPConfig
   { XP.font = "xft:Sans:pixelsize=14"
   , XP.height = 20
   , XP.position = XP.Top
   , XP.borderColor = "#404040"
   , XP.fgColor = "#F5f5f5"
   , XP.bgColor = "#494949"
   , XP.fgHLight = "#F5f5f5"
   , XP.bgHLight = "#4169e1"
   , XP.promptBorderWidth = 1
   , XP.searchPredicate = isInfixOf . (map toLower)
 }

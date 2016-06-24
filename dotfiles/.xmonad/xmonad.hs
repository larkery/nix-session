{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

import XMonad hiding ( (|||) )
import qualified XMonad.StackSet as W
import XMonad.Config.Desktop
import XMonad.Util.EZConfig

import XMonad.Util.NamedWindows

import XMonad.Hooks.Place
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops (fullscreenEventHook)
import XMonad.Hooks.ManageDocks ( ToggleStruts (ToggleStruts) )

import XMonad.Layout.NoBorders
import qualified XMonad.Layout.Renamed as Ren
import qualified XMonad.Layout.MouseResizableTile as MRT
import qualified XMonad.Layout.BoringWindows as Boring
import qualified XMonad.Layout.LimitWindows as Limit
import qualified XMonad.Layout.VarialColumn as VC
import XMonad.Layout.LayoutCombinators ( (|||), JumpToLayout (JumpToLayout) )

import qualified XMonad.Prompt as XP
import qualified XMonad.Prompt.Shell as XPS

import XMonad.Actions.RotSlaves
import qualified XMonad.Actions.DwmPromote as DWM
import XMonad.Actions.FindEmptyWorkspace
import qualified XMonad.Actions.CycleWS as C
import XMonad.Actions.Warp (warpToWindow)
import qualified XMonad.Actions.GridSelect as GS
import XMonad.Actions.WindowBringer (bringWindow)
import XMonad.Actions.CycleRecentWS

import System.Exit
import Data.List (isInfixOf, (\\))
import Data.Char (toLower)
import Data.Maybe (isJust, fromJust, listToMaybe, fromMaybe)

import XMonad.Layout.CountLabel (addCount)
import XMonad.Util.AccelerateScroll (accelerate)
import XMonad.Actions.BringFrom (bringFrom)

import System.Taffybar.Hooks.PagerHints (pagerHints)

as n x = Ren.renamed [Ren.Replace n] x

layout = XMonad.Layout.NoBorders.smartBorders $
         addCount $
         Boring.boringAuto $ (tiled ||| twocol ||| full)
  where
    tiled = as "s" $ VC.varial
    twocol = as "d" $ Limit.limitWindows 2 $ VC.varial
    full = as "f" $ Full

-- bindings which work in certain layouts
inLayout :: [(String, X ())] -> X () -> X ()
inLayout as d =
  do lname <- gets (description . W.layout . W.workspace . W.current . windowset)
     let lname' = head $ words lname
     fromMaybe d $ lookup lname' as

focusUp = inLayout [("f", windows W.focusUp)] Boring.focusUp
focusDown = inLayout [("f", windows W.focusDown)] Boring.focusDown

manageHooks config = config {
  manageHook = (manageHook config) <+>
               placeHook (withGaps (16,16,16,16) (underMouse (0.5, 0.2))) <+>
               composeAll
               [
                 isDialog --> doFloat,
                 isFullscreen --> doFullFloat
               ]
  }

eventHooks config = config {
  handleEventHook = (handleEventHook config) <+>
                    fullscreenEventHook
  }

bscrollup = 4
bscrolldown = 5
bccw = 9
bcw = 10

typeKey :: String -> X ()
typeKey k = spawn $ "xdotool key --clearmodifiers " ++ k

minWs = "*"
wsNames = ["q","w","e","r"] ++ (map show [5..9]) ++ [minWs]

interestingWS = C.WSIs $
  do hs <- gets (map W.tag . W.hidden . windowset)
     return (\w -> (W.tag w /= minWs) &&
                   (isJust $ W.stack w) &&
                   (W.tag w `elem` hs))

main = do
  xmonad $
    manageHooks $
    eventHooks $
    pagerHints $
    desktopConfig
    { modMask     = mod4Mask
    , layoutHook = desktopLayoutModifiers $
                   layout
    , workspaces = wsNames
    , focusedBorderColor = "#00bfff"
    , borderWidth = 2
    }
    `additionalMouseBindings`
    -- this is mod scrollwheel
    [
      ((0, bcw),  const $ accelerate bscrolldown),
      ((0, bccw), const $ accelerate bscrollup),
      ((mod4Mask, bcw), const $ typeKey "XF86AudioRaiseVolume"),
      ((mod4Mask, bccw), const $ typeKey "XF86AudioLowerVolume")
    ]
    `removeKeysP`
    [p ++ [n] | p <- ["M-", "M-S-"], n <- ['1'..'9']]
    `additionalKeysP`
    ([("M-<Escape>", spawn "xmonad --recompile; xmonad --restart"),
      ("M-S-<Escape>", io (exitWith ExitSuccess)),
      ("M-<Return>", DWM.dwmpromote),

      ("M-l", C.moveTo C.Prev interestingWS),
      ("M-;", C.moveTo C.Next interestingWS),
      ("M-S-y", tagToEmptyWorkspace),
      ("M-y", viewEmptyWorkspace),

      ("M-a e", spawn "emacsclient -c -n"),
      ("M-a w", spawn "vimb"),
      ("M-a q", spawn "qb"),
      ("M-a d", spawn "dmenu_run"),
      ("M-a M-a", XPS.shellPrompt prompt),
      ("M-a h", spawn "systemctl hibernate"),
      ("M-a l", spawn "systemctl suspend"),
      ("M-a m", spawn "xdg-open mailto:"),
      ("M-a M-m", spawn "notmuch new"),
      ("M-a s", spawn "xterm"),

      ("M-k", kill),

      ("M-g",   goToSelected gsconfig),
      ("M-b",   bringSelected gsconfig),
      ("M-S-b", bringMinned gsconfig),

      ("M-v",   sendMessage ToggleStruts),

      ("M-z", cycleRecentWS [xK_Super_L] xK_z xK_x),

      ("M-m", windows $ W.shift minWs),
      ("M-S-m", bringFrom minWs),

      ("M-p", focusUp),
      ("M-n", focusDown),

      ("M-,", rotSlavesUp),
      ("M-.", rotSlavesDown),

      ("M--",   C.nextScreen >> warpToWindow 0.1 0.1),
      ("M-S--", C.shiftNextScreen >> C.nextScreen >> warpToWindow 0.1 0.1),
      ("M-=",   C.swapNextScreen),

      ("M-S-n", withFocused $ \w -> sendMessage $ VC.DownOrRight w),
      ("M-S-p", withFocused $ \w -> sendMessage $ VC.UpOrLeft w),

      ("M-c M-c", withFocused $ \w -> sendMessage $ VC.ToNewColumn w),
      ("M-c c",   withFocused $ \w -> sendMessage $ VC.GrabColumn w),
      ("M-c h",   withFocused $ \w -> sendMessage $ VC.GrabRow w),
      ("M-c e",   withFocused $ \w -> sendMessage $ VC.EqualizeColumn 1 w),
      ("M-c q",   withFocused $ \w -> sendMessage $ VC.EqualizeColumn 0.5 w),

      ("M-u",   withFocused $ \w -> sendMessage $ VC.Embiggen deltaw 0 w),
      ("M-S-u", withFocused $ \w -> sendMessage $ VC.Embiggen (-deltaw) 0 w),
      ("M-i",   withFocused $ \w -> sendMessage $ VC.Embiggen 0 deltah w),
      ("M-S-i", withFocused $ \w -> sendMessage $ VC.Embiggen 0 (-deltah) w)
     ]
     ++
     [("M-" ++ k, ((sendMessage $ JumpToLayout k))) | k <- ["s","d","f"]]
     ++
     [("M-" ++ (show n), (windows $ W.greedyView ws)) | (n, ws) <- zip [1..9] wsNames]
     ++
     [("M-" ++ k, (windows $ W.greedyView ws)) | (k, ws) <- zip ["q","w","e","r"] wsNames]
     ++
     [("M-S-" ++ (show n), (windows $ W.shift ws)) | (n, ws) <- zip [1..9] wsNames]
    )
  where
    deltaw :: Rational
    deltaw = 0.2
    deltah :: Rational
    deltah = 0.2

prompt = XP.def
   { XP.font = "xft:Mono-12"
   , XP.height = 28
   , XP.position = XP.Top
   , XP.borderColor = "#AAAAAA"
   , XP.fgColor = "#F5f5f5"
   , XP.bgColor = "#494949"
   , XP.fgHLight = "#000000"
   , XP.bgHLight = "#ffffff"
   , XP.promptBorderWidth = 2
   , XP.searchPredicate = isInfixOf . (map toLower)
   , XP.maxComplRows = Just 10
   , XP.historySize = 100
   , XP.promptKeymap = XP.emacsLikeXPKeymap
   , XP.alwaysHighlight = True
 }

gsconfig = GS.def
  {
    GS.gs_navigate = GS.navNSearch,
    GS.gs_cellwidth = 256
  }

gridselectWindow :: (String -> Window -> Bool) -> GS.GSConfig Window-> X (Maybe Window)
gridselectWindow f gsconf = windowMap f >>= GS.gridselect gsconf

windowMap :: (String -> Window -> Bool) -> X [(String,Window)]
windowMap test = do
    ws <- gets windowset
    let ws' = [(W.tag wspace, w) | wspace <- W.workspaces ws, w <- W.integrate' (W.stack wspace),
               test (W.tag wspace) w]
    mapM mkPair ws'
      where mkPair :: (String, Window) -> X (String, Window)
            mkPair (sname, wid) = do
              name <- getName wid
              return (sname ++ ": " ++ (show name), wid)

decorateName' :: Window -> X String
decorateName' w = do
  fmap show $ getName w

withSelectedWindow :: (String -> Window -> Bool) -> (Window -> X ()) -> GS.GSConfig Window -> X ()
withSelectedWindow f callback conf = do
    mbWindow <- gridselectWindow f conf
    case mbWindow of
        Just w -> callback w
        Nothing -> return ()

-- | Brings selected window to the current workspace.
bringSelected :: GS.GSConfig Window -> X ()
bringSelected c = do
  cur <- gets (W.tag . W.workspace . W.current . windowset)
  withSelectedWindow (\t _ -> t /= cur) (\w -> do
    windows (bringWindow w)
    XMonad.focus w
    windows W.shiftMaster) c

-- | Switches to selected window's workspace and focuses that window.
goToSelected :: GS.GSConfig Window -> X ()
goToSelected c = do
  cur <- gets (W.tag . W.workspace . W.current . windowset)
  withSelectedWindow (\t _ -> t /= minWs && t/=cur) (windows . W.focusWindow) c

bringMinned :: GS.GSConfig Window -> X ()
bringMinned = withSelectedWindow (\t _ -> t == minWs) $ \w -> do
    windows (bringWindow w)
    XMonad.focus w
    windows W.shiftMaster

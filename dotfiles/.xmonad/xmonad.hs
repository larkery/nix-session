import XMonad hiding ( (|||) )
import XMonad.Layout.MouseResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.LayoutCombinators
import XMonad.Util.Themes
import XMonad.Config.Desktop
import System.Taffybar.Hooks.PagerHints (pagerHints)

main = xmonad $
  pagerHints $
  desktopConfig
    { modMask     = mod4Mask
    , layoutHook = desktopLayoutModifiers $
                   mouseResizableTile {draggerType = FixedDragger 2 6} |||
                   mouseResizableTile {draggerType = FixedDragger 2 6,
                                       isMirrored = True} |||
                   (tabbed shrinkText (theme smallClean))
    }

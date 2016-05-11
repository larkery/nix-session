import System.Taffybar

import System.Taffybar.Pager
import System.Taffybar.TaffyPager
import System.Taffybar.Systray
import System.Taffybar.SimpleClock
import System.Taffybar.FreedesktopNotifications
import System.Taffybar.Weather

import System.Taffybar.Widgets.PollingBar
import System.Taffybar.Widgets.PollingGraph

import System.Information.Memory
import System.Information.CPU

import Graphics.UI.Gtk.General.RcStyle (rcParseString)
import System.Taffybar.Battery

import System.Taffybar.NetMonitor

memCallback = do
  mi <- parseMeminfo
  return [memoryUsedRatio mi]

cpuCallback = do
  (userLoad, systemLoad, totalLoad) <- cpuLoad
  return [totalLoad, systemLoad]

main = do
  let memCfg = defaultGraphConfig { graphDataColors = [(1, 0, 0, 1)]
                                  , graphLabel = Just "mem"
                                  }
      cpuCfg = defaultGraphConfig { graphDataColors = [ (0, 1, 0, 1)
                                                      , (1, 0, 1, 0.5)
                                                      ]
                                  , graphLabel = Just "cpu"
                                  }
      clockCfg = "<span fgcolor='white'>%a %b %_d %H:%M</span>"
  let clock = textClockNew Nothing clockCfg 1
      note  = notifyAreaNew defaultNotificationConfig

      mem   = pollingGraphNew memCfg 2 memCallback
      cpu   = pollingGraphNew cpuCfg 2 cpuCallback
      batt  = batteryBarNew defaultBatteryConfig 10
      nm    = netMonitorNew 2 "wlan0"
      tray  = systrayNew
      font  = "Monospace 8"
      pager = taffyPagerNew defaultPagerConfig
              {
                activeWindow = escape
              , activeWorkspace  = colorize "black" "white" . wrap " " " " . escape
              , visibleWorkspace = colorize "black" "gray" . wrap " " " " . escape
              , hiddenWorkspace = colorize "gray" "" . wrap " " " " . escape
              , urgentWorkspace = colorize "white" "red" . wrap " " " " . escape
              }

  defaultTaffybar defaultTaffybarConfig
                  { barHeight = 16
                  , startWidgets = [ pager, note ]
                  , endWidgets = [ tray, clock
                                 , batt
                                 , mem, cpu, nm
                                 ]
                  }

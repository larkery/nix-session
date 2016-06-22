import System.Taffybar

import System.Taffybar.Pager
import System.Taffybar.TaffyPager
import System.Taffybar.Systray
import System.Taffybar.SimpleClock
import System.Taffybar.Weather

import System.Taffybar.Widgets.PollingBar
import System.Taffybar.Widgets.PollingGraph

import System.Information.Memory
import System.Information.CPU

import Graphics.UI.Gtk.General.RcStyle (rcParseString)
import System.Taffybar.Battery

import System.Taffybar.NetMonitor

import Data.List.Split (splitOn)
import Data.Ratio
import qualified Data.Map as M
import Data.Maybe
import qualified Debug.Trace as D

import Control.Applicative

memCallback = do
  mi <- parseMeminfo
  return [memoryUsedRatio mi]

cpuCallback = do
  (userLoad, systemLoad, totalLoad) <- cpuLoad
  return [totalLoad, systemLoad]


readBatt = do
  thelines <- readFile "/sys/class/power_supply/BAT0/uevent"
  let result = map (break (== '=')) $ lines $ thelines
  return $ M.fromList $ map (\(a, b) -> (a, drop 1 b)) result


battStat :: M.Map String String -> Maybe (Integer, Integer, Integer)
battStat m =
  let a = do full <- M.lookup "POWER_SUPPLY_CHARGE_FULL" m
             now <- M.lookup "POWER_SUPPLY_CHARGE_NOW" m
             current <- M.lookup "POWER_SUPPLY_CURRENT_NOW" m
             return $ (read now :: Integer, read full :: Integer, read current :: Integer)
      b = do full <- M.lookup "POWER_SUPPLY_ENERGY_FULL" m
             now <- M.lookup "POWER_SUPPLY_ENERGY_NOW" m
             current <- M.lookup "POWER_SUPPLY_POWER_NOW" m
             return $ (read now :: Integer, read full :: Integer, read current :: Integer)
  in a <|> b

battsum :: IO Double
battsum = do
  dat <- readBatt
  return $ fromMaybe 0 $ do (n, f, r) <- battStat dat
                            let per =  (n % f)
                            return $ fromRational per


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
      bat = pollingBarNew defaultBatteryConfig 10 battsum

      cpu   = pollingGraphNew cpuCfg 2 cpuCallback
      nm    = netMonitorNew 2 "wlan0"
      tray  = systrayNew
      font  = "Monospace 8"
      pager = taffyPagerNew defaultPagerConfig
              {
                activeWindow = escape
              , activeWorkspace  = colorize "black" "white" . wrap " " " " . escape
              , visibleWorkspace = colorize "white" "gray" . wrap " " " " . escape
              , hiddenWorkspace = colorize "white" "" . wrap " " " " . escape
              , urgentWorkspace = colorize "white" "red" . wrap " " " " . escape
              }

  defaultTaffybar defaultTaffybarConfig
                  { barHeight = 18
                  , startWidgets = [ pager ]
                  , endWidgets = [ tray, clock
                                 , bat
                                 , cpu, nm
                                 ]
                  }

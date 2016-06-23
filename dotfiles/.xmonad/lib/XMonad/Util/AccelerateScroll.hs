{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module XMonad.Util.AccelerateScroll where

import XMonad
import qualified XMonad.Util.ExtensibleState as XS
import Data.Time.Clock.POSIX (getPOSIXTime, POSIXTime)

import qualified Graphics.X11.XTest as XTest
import Control.Concurrent (threadDelay)
import Control.Monad

-- the last series of scroll events
data ScrollEvents = ScrollEvents Button [POSIXTime] deriving Typeable
instance ExtensionClass ScrollEvents where
  initialValue = ScrollEvents 0 []

accelerate simbutton = do
  (ScrollEvents last times) <- XS.get
  -- now we want to produce a rate
  now <- io getPOSIXTime
  let times' = now:(filter (\x -> now - x < 0.8) times)
  let rate = (fromIntegral $ length times')
  -- now we want to generate some number of scroll events, based on some kind of acceleration function
  let n = 1 + if simbutton == last then (floor ((rate/2.0) ** 1.25)) else 0
  withDisplay $ \d -> io $ replicateM_ n $ XTest.fakeButtonPress d simbutton >> threadDelay 5
  XS.put (ScrollEvents simbutton times')

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module XMonad.Layout.CountLabel where

import XMonad
import XMonad.Layout.LayoutModifier
import qualified XMonad.StackSet as W

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

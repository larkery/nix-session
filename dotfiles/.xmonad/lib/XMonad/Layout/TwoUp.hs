{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses, PatternGuards, TypeSynonymInstances #-}

module XMonad.Layout.TwoUp where

import XMonad.Layout.LayoutModifier
import XMonad
import XMonad.StackSet
import Data.Maybe

data TwoUp a = TwoUp Bool (Maybe Window)
  deriving (Show, Read)

instance LayoutModifier TwoUp a where
  -- hax
  modifyLayoutWithUpdate state@(TwoUp enabled lastFocus) w r =
    if not enabled then
      do result <- runLayout w r
         return (result, Nothing)
    else
      do let stack' = do Stack f u d <- stack w
                         return $ Stack f (take 1 u) (if ([] == u) then take 1 d else [])
             focus' = do Stack f u d <- stack w
                         if u == [] then lastFocus
    
twoUp :: l a -> ModifiedLayout TwoUp l a
twoUp = ModifiedLayout (TwoUp False Nothing)

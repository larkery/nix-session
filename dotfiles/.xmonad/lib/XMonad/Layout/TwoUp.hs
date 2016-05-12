{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses, PatternGuards, TypeSynonymInstances #-}

module XMonad.Layout.TwoUp where

import XMonad.Layout.LayoutModifier
import XMonad
import XMonad.StackSet
import Data.Maybe

data TwoUp a = TwoUp Bool
  deriving (Show, Read, Eq)

data Toggle = Toggle deriving (Read, Show, Typeable)
instance Message Toggle

toggle = sendMessage Toggle

instance LayoutModifier TwoUp a where
  handleMess (TwoUp state) msg 
    | (Just Toggle) <- fromMessage msg = return $ Just $ TwoUp (not state)
    | otherwise = return Nothing

  modifyLayoutWithUpdate state@(TwoUp enabled) w r =
    if not enabled then
      do result <- runLayout w r
         return (result, Nothing)
    else
      let stack' = do st@(Stack f u d) <- stack w
                      return $
                        if length u + length d < 2 then st
                        else case (reverse u, f, d) of ([], _, []) -> st
                                                       ([], f, (x:xs)) -> Stack f [] [x]
                                                       ([x], f, _) -> Stack f [x] []
                                                       ((x:xs), f, _) -> Stack x [] (take 1 xs)
                                                       (_, _, _) -> st -- just in case
          w' = w { stack = stack' }
          enabled' = (length $ take 3 $ integrate' $ stack w) == 3
      in do result <- runLayout w' r
            return (result, Just $ TwoUp enabled')                        

  modifierDescription (TwoUp True) = "2up"
  modifierDescription (TwoUp False) = ""
    
twoUp :: l a -> ModifiedLayout TwoUp l a
twoUp = ModifiedLayout (TwoUp False)


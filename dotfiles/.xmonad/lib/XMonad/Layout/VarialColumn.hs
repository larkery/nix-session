{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses, PatternGuards, TypeSynonymInstances #-}

module XMonad.Layout.VarialColumn where

import qualified XMonad.StackSet as W
import XMonad hiding (tile, splitVertically, splitHorizontallyBy)

import qualified Data.Sequence as S
import Data.Sequence ( (><), (|>) )
import qualified Data.Foldable as F
import Data.Maybe
import Data.Int (Int32)

data Axis = V | H

type Col = S.Seq Rational
data Cols = Cols (S.Seq Rational) (S.Seq Col)
-- Layout State

data LS a = LS
          {
            cols :: Cols,
            -- ^ where the splits are

            gap :: Int,
            -- ^ gap between wins

            balanceColumns :: Int,
            insertColumns :: Int
          }

ins :: a -> Int -> S.Seq a -> S.Seq a
ins a n s = ((S.take n s) |> a) >< (S.drop n s)

ins1 = ins 1
insc = ins newCol

newCol = S.singleton 1

-- resize the thing at the position to have the given size in a way
-- that preserves the total and doesn't make anything less than 0.1
changeS :: Int -> Rational -> S.Seq Rational -> S.Seq Rational
changeS n r' s
  | null s = s
  | length s == 1 = s
  | otherwise = let lim = 0.1
                    a0 = S.index s n
                    a1 = S.index s (n+1)
                    r = min (a0 + a1 - lim) $ max r' lim in
                  S.update n r $ S.update (n+1) (r - a0 + a1) s

-- insert a new column at n
insertCol :: Int -> LS a -> LS a
insertCol n s@(LS {cols = (Cols rs cs)}) = s { cols = cols' }
  where cols' = Cols (ins1 n rs) (insc n cs)

-- insert a new row in column c at existing row n
insertRow :: Int -> Int -> LS a -> LS a
insertRow c r s@(LS {cols = (Cols rs cs)}) = s { cols = cols' }
  where cols' = Cols rs $ S.adjust (\x -> ins1 r x) c cs

-- also need to handle the deletion of a row / column. the
-- values need rebalancing to equal the total, or the next window needs to get it.

-- LAYOUT ALGO
layout :: LS a -> Rectangle -> W.Stack a -> ([(a, Rectangle)], Maybe (LS a)) 
layout ls screen stack =
  let ls'    = update ls stack
      rects  = windowRects (fromMaybe ls ls') screen stack
      ls''   = fmap (addDraggerRects screen) ls' in
    (rects, ls'')

-- handle windows added/removed
update :: LS a -> W.Stack a -> Maybe (LS a)
update ls@(LS { cols = cs }) stack
  | newCount < oldCount = Nothing
  | newCount > oldCount = Nothing
  | otherwise = Nothing
  where
    newCount = length $ W.integrate stack
    oldCount = windowCount ls

windowCount :: LS a -> Int
windowCount LS { cols = (Cols _ a) } = F.sum $ fmap S.length a

-- put the windows in the screen rectangle
windowRects :: LS a -> Rectangle -> W.Stack a -> [(a, Rectangle)]
-- step 1: cut the big rectangle into column rectangles
-- step 2: cut each column rectangle into row rectangles
-- step 3: zip the rectangles with the windows
windowRects (LS { cols = (Cols ws rs), gap = g }) screen stack =
  let allWindows = W.integrate stack
      colRectangles = cutup H g screen ws
      winRectangles = concatMap id $ zipWith (cutup V g) colRectangles $ F.toList rs
  in zip allWindows winRectangles

-- chop a rectangle up into subrectangles on its axis
cutup :: Axis -> Int -> Rectangle -> S.Seq Rational -> [Rectangle]
cutup ax g screen slices
  | total == 0 = []
  | total == 1 = [screen]
  | otherwise = F.toList rects
  where
    total   = fromIntegral $ S.length slices
    slices' = S.scanl (+) 0 $ fmap (flip (/) total) slices
    screen' = rot ax screen
    rects   = fmap ((rot ax) . (sub screen')) $ S.zip slices' $ S.drop 1 slices'

    rot H r = r
    rot V (Rectangle x y w h) = Rectangle y x h w
    
    sub :: Rectangle -> (Rational, Rational) -> Rectangle 
    sub (Rectangle x y w h) (l, r)= Rectangle x' y w' h
      where
        w' :: Dimension
        w' = floor ((fromIntegral w) * (r - l)) - (fromIntegral g)
        x' :: Position
        x' = x + (floor ((fromIntegral w) * l) :: Position) + (if l > 0 then (fromIntegral g) else 0)

-- cook up the locations for all the draggers
addDraggerRects :: Rectangle -> LS a -> LS a
addDraggerRects _ a = a

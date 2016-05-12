{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses, PatternGuards, TypeSynonymInstances #-}

module XMonad.Layout.MouseResizableColumns where

import Data.List (elemIndex, findIndex)
import Data.Maybe
import qualified XMonad.StackSet as W
import XMonad hiding (tile, splitVertically, splitHorizontallyBy)

-- a layout which may have several columns, each being mouse resizable
-- a column has a count of windows and a width
-- each frame in a column has a height

data Column = Column
  {
    width :: Rational,
    capacity :: Int,
    rows :: [Rational]
  } deriving (Show, Read)

data MouseResizableColumns a = MRC
  {
    columns :: [Column]
  } deriving (Show, Read)

mrc = MRC { columns = [
              Column { width = 1, capacity = 1, rows = [] },
              Column { width = 1, capacity = 1, rows = [] },
              Column { width = 0.3, capacity = 1, rows = [] }
              ]
          }

-- todo: handle case where middle column is unbounded
-- and left and right are fixed.

-- todo: handle mouses

-- useful messages to implement are perhaps
--  * grow/shrink focused window horizontally/vertically
--  * shift focused window into previous/next column
data Direction = L | R
data MRCMessage = MoveWindow Direction
instance Message MRCMessage

moveWindow :: Direction -> X ()
moveWindow d = sendMessage $ MoveWindow d 

instance LayoutClass MouseResizableColumns Window where
  description state = (show (length (columns state))) ++ "C"

  -- if I make pureLayout into doLayout instead
  -- I could store the real dimensions into the result
  -- which would be nice.
  
  doLayout state screen (W.Stack w l r) =
    -- w is the focused window, l is the windows before it backwards, and r the windows after
    -- produce [(Window, Rectangle)]; the rectangles for the windows
    let windows = reverse l ++ w:r -- all the windows
        nwindows = length windows
        -- these are the number of windows to go in each column
        splits = map capacity (columns state)
        totalCapacity = sum splits
        -- if we don't have room for all the windows, we need to make some room
        -- on the end of the last column
        columns' = if totalCapacity < nwindows then
                     reverse $ let (lc:rc) = reverse (columns state) in
                                 (lc { capacity =
                                       (capacity lc) +
                                       (nwindows - totalCapacity)}) :rc
                                 -- todo: maybe strip columns if they are not being used
                   else columns state
        -- now we will split the list of windows into sublists of this size
        -- now we can determine the column's widths
        columnWindows = splitAtMany (map capacity columns') windows
        -- if there are not enough columns, we modify the final column
        -- to have more capacity.
        columnWidths = map width $ take (length columnWindows) columns'
        totalWidth = sum columnWidths
        -- now we know the total fraction our columns are hoping for
        -- we can cut the horizontal dimension up accordingly to get
        -- the bases for rectangles
        Rectangle screenX screenY screenWidth screenHeight = screen

        -- chop is the function which chops up the real screen coordinates
        -- so that windows have the right sized boxes.
        chop :: Dimension -> Rational -> Position -> [Rational] -> [(Position, Dimension)]
        chop _ _ _ [] = []
        chop target total left (cw:rest) =
          let rw = floor $ (fromIntegral target) * cw / total
          in (left, rw):(chop target total (left + (fromIntegral rw)) rest)
        
        colPositions = chop screenWidth totalWidth screenX columnWidths
        -- finally we can chop each column up into its rows

        chopRow windows column (x, w) =
          let windowHeights = take (length windows) $ (rows column) ++ repeat 0.1
              totalHeight = sum windowHeights
              rowPositions = chop screenHeight totalHeight screenY windowHeights
              rowRects = map (\(y, h) -> Rectangle x y w h) rowPositions
          in zip windows rowRects
        result = [winbox | (win, col, colp) <- zip3 columnWindows columns' colPositions,
                                     winbox <- chopRow win col colp]
        -- now we have worked everything out, we also want to produce a new state
        -- whose sizing results are what we just computed.
        -- what do we do if we added a new window?
    in do return (result, Just $ state { columns = columns' })

  -- message handling
  handleMessage state msg
    | Just (MoveWindow d) <- fromMessage msg =
        do st <- gets (W.stack . W.workspace . W.current . windowset)
           -- I could make this work only on the focused window
           -- that makes things easier
           return $ do (W.Stack w l r) <- st
                       let windex = 1 + length l
                       return $ state { columns = shiftColumn 0 windex d $ columns state }
           
    | otherwise = return $ Just state

newColumn = (Column { width = 0.1, capacity = 1, rows = []} )

shiftColumn _ _ _ [] = []
shiftColumn _ _ L (c:[]) = newColumn:(shrink c)
shiftColumn _ _ R (c:[]) = (shrink c) ++ [newColumn]
shiftColumn a w L (c1:c2:cs)
  | w <= (a + cc1) = newColumn : ((shrink c1) ++ c2:cs)
  | w <= (a + cc1 + cc2) = (grow c1):((shrink c2) ++ cs)
  | otherwise = c1:(shiftColumn (a + cc1) w L (c2:cs)) 
    where cc1 = (capacity c1)
          cc2 = (capacity c2)
shiftColumn a w R (c1:c2:cs)
  | (a < w) && (w <= (a + cc1)) = (shrink c1) ++ (grow c2):cs
  | otherwise = c1:(shiftColumn (a + cc1) w R (c2:cs))
    where cc1 = (capacity c1)
          
shrink column =
  let result = column {capacity = max 0 $ (capacity column) - 1} in
    if 0 == capacity result then [] else [result]

grow   column = column {capacity = (capacity column) + 1}

-- cuts the second list up into chunks of sizes in the first list
-- until it's exhausted. will not produce the extra empty chunks
-- if it exhausts.

splitAtMany :: [Int] -> [a] -> [[a]]
splitAtMany (0:is) xs = [] : (splitAtMany is xs)
splitAtMany is (x:[]) = [[x]]
splitAtMany (i:is) (x:xs) = let r:rs = splitAtMany ((i-1):is) xs in (x:r):rs
-- all failure patterns are the same
splitAtMany _ _ = []


-- TODO how should this work??

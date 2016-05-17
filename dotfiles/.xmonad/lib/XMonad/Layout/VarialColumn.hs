{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses, PatternGuards, TypeSynonymInstances #-}

module XMonad.Layout.VarialColumn where

import qualified XMonad.StackSet as W
import XMonad hiding (tile, splitVertically, splitHorizontallyBy)
import Data.Maybe
import qualified Data.Set as S
import XMonad.Util.XUtils
import Data.Int (Int32)

data OnInsert = Ignore | Focused Int
  deriving (Show, Read, Eq) -- ignore prevents column growth

data VarialColumn a = V
  {
    columns :: [Column],
    insert :: OnInsert,
    rebalance :: Int,
    gap :: Int,
    draggers :: [WDragger]
  } deriving (Show, Read)

data Column = C
  {
    width :: Rational,
    rows :: [Rational]
  } deriving (Show, Read)


-- the drag event produces dx/dy
-- this is in screen space
-- we have stuff which is instead in layout space
-- it wouldn't be hard to make layout space = screen space
-- but is that wise?

data Dragger = ColumnDragger Int Position Dimension |
               RowDragger Int Int Position Dimension
  deriving (Show, Read)
type WDragger = (Window, Dragger)

data VarialMessage =
  Balance          | -- not sure this is useful
  NewColumn | -- pop the focused window into a new column to the left
  EatColumn -- make the focused window use up its whole column, moving
            -- windows above left and below right.

instance Message VarialMessage

varial = V
  {
    columns = [],
    insert = Focused 2, -- this is how many columns we will go up to on a new window
    rebalance = 1, -- this is hysteresis; if we go down to 1 window, we attempt to rebalance.
    gap = 2,
    draggers = []
  }

capacity (C { rows = r }) = length r

instance LayoutClass VarialColumn Window where
  description _ = "Varial"
  doLayout state screen stack =
    do destroyDraggers state
       let (rectangles, draggers', mnewstate) = layout state screen stack
       draggers'' <- mapM (addWindow (fromIntegral (gap state))) draggers'
       let newstate = (fromMaybe state mnewstate)
                      { draggers = draggers'' }
       -- the result is fine, but we also need to add rectangles for all the draggers
       -- oddly not doing this doesn't seem to cause any problems???
       return $ (rectangles, Just newstate)

  -- message handling
    -- TODO: event handling for draggers
  handleMessage state msg
    | (Just Balance) <- fromMessage msg =
        return $ Just $ balance state
    | (Just NewColumn) <- fromMessage msg =
        handleNewColumn state
    | (Just e) <- fromMessage msg :: Maybe Event =
        handleResize e (draggers state) >> return Nothing
    | (Just Hide) <- fromMessage msg =
        destroyDraggers state >> return (Just $ state { draggers = [] })
    | (Just ReleaseResources) <- fromMessage msg =
        destroyDraggers state >> return (Just $ state { draggers = [] })
    | otherwise = return Nothing

-- given some draggers, make up windows for them.
addWindow :: Int32 -> Dragger -> X WDragger
addWindow gap d@(ColumnDragger cn x h) = do
  win <- makeDraggerWindow xC_sb_h_double_arrow (Rectangle (x - gap) 0 (fromIntegral (gap * 2)) h)
  return (win, d)
--addWindow a = return $ 

-- resize works by sending ourself a message about the resize
handleResize :: Event -> [WDragger] -> X ()
handleResize ButtonEvent { ev_window = ew, ev_event_type = et } draggers
  | et == buttonPress, Just x <- lookup ew draggers = case x of
      ColumnDragger col colpos _ ->
        
        return ()
      _ -> return ()

handleResize _ _ = return ()

destroyDraggers x = mapM_ (deleteWindow . fst) $ draggers x

times :: Int -> X () -> X ()
times n a
  | n <= 0 = return ()
  | otherwise = do times (n - 1) a
                   a

handleNewColumn :: VarialColumn Window -> X (Maybe (VarialColumn Window))
handleNewColumn state =
  do st <- gets (W.stack . W.workspace . W.current . windowset)
     let change = do (W.Stack f u d) <- st
                     let fi = length u
                     let (swaps, columns') = fiddle fi $ columns state
                     return $ (swaps, state { columns = columns'})
                       where -- I should learn zippers
                         fiddle :: Int -> [Column] -> (Int, [Column])
                         fiddle fi [] = (0, [])
                         fiddle fi (c:cs)
                           | fi < cap && cap > 1 = (fi, newColumn:((setCapacity (cap - 1) c):cs))
                           | fi < cap = (0, c:cs)
                           | otherwise = let (x, y) = fiddle (fi - cap) cs in (x, c:y)
                           where cap = capacity c
         hops = fromMaybe 0 $ fmap fst change
     -- do swapUp hops times
     times hops $ windows W.swapUp
     
     return $ fmap snd change

-- given layout configuration, and a screen rectangle, and the workspace contents
-- run the layout and produce rectangles for each window and the revised state
layout :: VarialColumn a -> Rectangle -> W.Stack a -> ([(a, Rectangle)], [Dragger], Maybe (VarialColumn a))
layout state screen s@(W.Stack f u d) =
  -- check for new windows and update layout
  let newState = updateState state s
      runState = fromMaybe state newState
      (rectangles, draggers) = createRectangles runState screen s
  in (rectangles, draggers, newState)

-- handle addition and removal of windows using the two functions above.
updateState :: VarialColumn a -> W.Stack a -> Maybe (VarialColumn a)
updateState v@(V {columns = cs, insert = ip, rebalance = rb}) st@(W.Stack f u d)
  -- windows removed; delete trailing
  -- could delete near focused instead, but that seems iffy?
  | newCount < oldCount = Just $ balance $ removeFocus v (length u) (oldCount - newCount)
  -- windows added, focus policy
  | (newCount > oldCount) =
    case ip of (Focused _) -> Just $ insertFocus v (length u) (newCount - oldCount)
               _ -> Nothing
  -- no change, or policy not relevant
  | otherwise = Nothing
  where
    newCount = length $ W.integrate st
    oldCount = windowCount v

balance v@(V {columns = cs, rebalance = rb})
  | ncs <= rb = v { columns = introduce rb cs } 
  | otherwise = v
    where ncs = length cs
          wc = windowCount v
          introduce _ [] = []
          introduce 0 a = a
          introduce n ((c@(C {rows = [_]})):cs) = c:(introduce n cs)
          introduce n (c:cs) =
            if cap <= n then (take cap $ repeat newColumn) ++ (introduce (n - cap) cs)
            else (take n $ repeat newColumn) ++ (setCapacity (cap - n) c):cs
            where cap = capacity c

windowCount v = sum $ map capacity $ columns v

newColumn = C { width = 0.1, rows = [0.1] }
setCapacity n c = c { rows = take n $ (rows c) ++ (repeat 0.1)} 

-- handle the removal of a window close to the focused window
-- at the moment this will never increase the number of columns
removeFocus v fi n = v { columns = shrink' fi n (columns v) }
  where
    shrink' _ _ [] = []
    shrink' _ 0 a  = a

    shrink' fi n a@(c:cs)
      | n < 0 = a
      | cap > (fi + 1 - n) = if n >= cap then shrink' (fi - cap) (n - cap) cs
                         else (setCapacity (cap - n) c):cs
      | otherwise = c:(shrink' (fi - cap) n cs)
        where
          cap = capacity c

-- handle the introduction of a new window close to the focused window
insertFocus v@(V {columns = cols, insert = Focused k}) fi n =
  let spareColumns = max 0 (k - length cols)
      addColumns = (min spareColumns n)
      addRows = max 0 (n - addColumns)
  in
    v { columns = insertRows fi addRows $ insertColumns fi addColumns (columns v) }
  where
    insertColumns _ 0 a  = a
    insertColumns _ n [] = take n $ repeat newColumn
    insertColumns fi n (c:cs)
      | cap > fi = (take n $ repeat newColumn) ++ (c:cs)
      | otherwise = c:(insertColumns (fi - cap) n cs)
        where cap = capacity c

    insertRows _ 0 a  = a
    insertRows _ n [] = [setCapacity n newColumn]
    insertRows fi n (c:cs)
      | cap > fi = (setCapacity ((capacity c) + n) c):cs
      | otherwise = c:(insertRows (fi - cap) n cs)
        where cap = capacity c

-- produce the display rectangles for the layout
-- this is fairly easy since all the other work is done elsewhere
createRectangles :: VarialColumn a -> Rectangle -> W.Stack a -> ([(a, Rectangle)], [Dragger])
createRectangles v@(V {columns = cs, gap = g}) (Rectangle sx sy sw sh) st@(W.Stack f u d) =
  let columnSpans = cutup g (fromIntegral sx) (fromIntegral sw) $ map width cs
      -- each pile of windows that's going to go in a column
      wins = W.integrate st
      splitWins = splitPlaces wins $ map capacity cs
      makeRect :: (Rational, Rational) -> (Rational, Rational) -> Rectangle
      makeRect (x, w) (y, h) = Rectangle
                               (floor x :: Position)
                               (floor y :: Position)
                               (floor w :: Dimension)
                               (floor h :: Dimension)
      columnDraggers = [ ColumnDragger i (floor colx :: Position) sh | (i, (colx, _)) <- zip [1..] $ drop 1 columnSpans ]
      layoutResult = [ (window, makeRect cspan vspan) | (pile, column, cspan) <- zip3 splitWins cs columnSpans,
                       (window, vspan) <- zip pile $
                                          cutup g (fromIntegral sy) (fromIntegral sh) $
                                          (rows column) ]
  in
    (layoutResult, columnDraggers)
    

cutup :: Int -> Rational -> Rational -> [Rational] -> [(Rational, Rational)]
cutup gap' start extent fractions =
  let gap = fromIntegral gap'
      fractot = sum fractions
      lefts' = scanl (+) start $ map ((*) (extent / fractot)) fractions
      rights' = reverse $ drop 1 lefts'
      shrink = flip (-) gap
      grow = (+) gap
      gl = (take 1 lefts') ++ (map grow (drop 1 lefts'))
      gr = reverse $ (take 1 rights') ++ (map shrink (drop 1 rights'))
  in
    zip gl $ zipWith (-) gr gl

splitPlaces :: [a] -> [Int] -> [[a]]
splitPlaces _ [] = []
splitPlaces as (0:is) = splitPlaces as is
splitPlaces as (i:is) = a0:(splitPlaces a1 is)
  where (a0, a1) = splitAt i as

-- Mouse resizing

-- we need to make an X window for each drag handle
-- then when we get a drag event on a drag handle, we need to find the right row/col and hack it.

makeDraggerWindow :: Glyph -> Rectangle -> X Window
makeDraggerWindow g (Rectangle x y w h) = withDisplay $ \d -> do
  win <- do
    rw <- asks theRoot
    let screen = defaultScreenOfDisplay d
        visual = defaultVisualOfScreen screen
        attrmask = cWOverrideRedirect
    io $ allocaSetWindowAttributes $ \a -> do
      set_override_redirect a True
      createWindow d rw x y w h 0 0 inputOnly visual attrmask a
  
  io $ selectInput d win (exposureMask .|. buttonPressMask)
  cursor <- io $ createFontCursor d g
  io $ defineCursor d win cursor
  io $ freeCursor d cursor
  showWindow win
  return win

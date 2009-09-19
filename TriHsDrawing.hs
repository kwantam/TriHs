
{- Copyright (C) 2009 Riad S Wahby <rsw@jfet.org>
 - 
-- This file is part of TriHs
-- 
-- $Id$
--
--  TriHs is free software.  It comes without any warranty, to
--  to the extent permitted by applicable law.  You can redistribute it
--  and/or modify it under the terms of the Do What The Fuck You Want To
--  Public License, Version 2, as published by Sam Hocevar.  See
--  the COPYING file or http://sam.zoy.org/wtfpl/COPYING for more details
 -
 -}

module TriHsDrawing where

import Graphics.Rendering.Cairo
import TriHsPieces
import Data.Array as DA

-- *** GENERAL DRAWING FUNCTIONS ***
-- these functions are for drawing the various tetris pieces in Cairo

drawTetrisBoard :: Double -> Double -> TetrisBoard -> Render ()
drawTetrisBoard dunitx dunity tb = 
            mapM (drawTetrisLine dunitx dunity) (DA.assocs tb) >> return ()

drawTetrisLine :: Double -> Double -> (Int,TetrisLine) -> Render ()
drawTetrisLine dunitx dunity (y,lx) = mapM drawFromLine (DA.assocs lx) >> return ()
  where drawFromLine (x,tbl) = drawTetrisBlock tbl dunitx dunity x y

drawTetrisBlock :: TetrisBlock -> Double -> Double -> Int -> Int -> Render ()
drawTetrisBlock tb dunitx dunity cx cy =
  if tb == Nil then return () else do
  setSourceTBlock tb
  rectangle (dunitx / 20 + dunitx * fromIntegral cx) 
            (dunity / 20 + dunity * fromIntegral cy)
            (dunitx - dunitx / 10)
            (dunity - dunity / 10)
  fill

drawTetrisPiece :: TetrisPiece -> Double -> Double -> Int -> Int -> Int -> Render ()
drawTetrisPiece tp dunitx dunity cx cy rot = 
  mapM (\(x,y) -> drawTetrisBlock tb dunitx dunity x y) sC >> return ()
    where (TPiece sC _ tb) = shiftRotateTetrisPiece (cx,cy) rot tp

-- set color based on TetrisBlock type
setSourceTBlock :: TetrisBlock -> Render ()
setSourceTBlock t = setSourceRGB tr tg tb
  where (tr,tg,tb) = tBlockToRGBd t

-- redraw the main window
reDraw :: Bool -> (Int,Int) -> TetrisGameState -> Render ()
reDraw doShad (x,y) tgS = do
  let (TBState cx cy rot tp) = blstate tgS
  let bstate = bdstate tgS
  let ddx = fromIntegral x / 10
  let ddy = fromIntegral y / 20
  let (TBState cx' cy' rot' tp') = blstate (gStateMoveBottom tgS)
  drawTetrisBoard ddx ddy bstate
  if doShad then drawTetrisPiece (tp' { block = Gray }) ddx ddy cx' cy' rot' else return ()
  drawTetrisPiece tp ddx ddy cx cy rot

-- redraw the preview window
-- this doesn't use the Bool, just put it there to
-- keep the type signature the same as the one above
preDraw :: Bool -> (Int,Int) -> TetrisGameState -> Render ()
preDraw _ (x,y) tgS = do
  let np = pnext tgS
  let ddx = fromIntegral x / 3
  let ddy = fromIntegral y / 4
  drawTetrisPiece np ddx ddy 1 3 0



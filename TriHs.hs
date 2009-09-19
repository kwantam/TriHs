
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

module Main where

import Data.Array ((!))
import Graphics.UI.Gtk hiding (fill)
import Graphics.Rendering.Cairo (Render(..))
import Graphics.UI.Gtk.Gdk.Events
import Control.Concurrent.MVar
import Control.Monad (liftM,mapM)
import System.Random
import TriHsPieces
import TriHsDrawing

-- *** HANDLER FUNCTIONS ***
-- these functions are the GTK handlers and their various helpers

-- if hID == 0, then we are "paused" and most keypresses are ignored
handleButtonPress :: Window -> DrawingArea -> MVar TetrisGameState ->
                     IO Bool -> Label -> Label -> Event -> IO Bool
handleButtonPress w d b tHnd lLn lSn ev = do
      gState <- readMVar b
      let mqd = modifyThenQueueDraw b d
      case (eventKeyChar ev,eventModifier ev,eventKeyName ev,hID gState == 0) of
           (Just 'x', [], _, False) -> mqd gStateRotateCW      -- only when not paused
           (Just 'z', [], _, False) -> mqd gStateMoveBottom >> tHnd >> return True -- "
           (_ ,   [],   "Up",False) -> mqd gStateRotateCCW          -- "
           (_,[],"XF86Forward",False) -> mqd gStateRotateCW         -- "
           (_,[], "XF86Back",False) -> mqd gStateMoveBottom >> tHnd >> return True -- "
           (_ ,   [], "Down",False) -> tHnd >> return True          -- "
           (_ ,   [], "Left",False) -> mqd gStateMoveLeft           -- "
           (_ ,   [],"Right",False) -> mqd gStateMoveRight          -- "
           (Just 'p', [],     _, _) -> togglePauseGame b tHnd       -- pause / unpause
           (Just 'r',[Control],_,_) -> startNewGame b lLn lSn w tHnd  -- ctrl-r : restart game
           (Just 'q',[Control],_,_) -> widgetDestroy w >> return True -- quit
           (_    ,    _ ,   _,   _) -> return False -- otherwise don't handle this press

-- pause game
togglePauseGame :: MVar TetrisGameState -> IO Bool -> IO Bool
togglePauseGame tgS tHnd = do
           gst <- readMVar tgS
           if hID gst == 0
            then do nHID <- timeoutAdd tHnd $ gtime gst
                    modifyMVar_ tgS (ioify $ \_ -> gst { hID = nHID })
                    return True
            else do timeoutRemove $ hID gst
                    modifyMVar_ tgS (ioify $ \_ -> gst { hID = 0 })
                    return True

-- start a new game
startNewGame :: MVar TetrisGameState -> Label -> Label -> Window -> IO Bool -> IO Bool
startNewGame tgS lLn lSn w tHnd = do
           gst <- readMVar tgS
           timeoutRemove $ hID gst               -- remove old timeout
           nGS <- genNewState                    -- new gamestate
           nHID <- timeoutAdd tHnd 1000          -- add new timeout
           modifyMVar_ tgS (ioify $ \_ -> nGS { hID = nHID }) -- save new state
           labelSetText lLn "0"                  -- reset lines count
           labelSetText lSn "0"                  -- reset score
           widgetQueueDraw w                     -- redraw screen
           return True

-- oof this is so ugly and imperative
-- this code makes me feel a little dirty
timerHandler :: DrawingArea -> DrawingArea -> Label -> Label -> MVar TetrisGameState -> IO Bool
timerHandler daMain daPrev lLn lSc mtgS = do
           tgs <- readMVar mtgS                     -- read game state
           let (collided,tgs') = stateDropPiece tgs -- drop the piece, detect collisions, &c
           if collided  -- if we got a collision, we have some splainin to do
            then do 
             newpiece <- genRandomPiece                            -- first, next piece
             let (nlines,tbd) = removeCompleteLines $ bdstate tgs' -- any complete lines?
             (tgs'',retval) <-                                     -- OK, now this is ugly
              if (tbd ! 0) /= emptyLine -- if there is anything in the top line,
               then                     -- game over!
                 return $ (tgs' { bdstate = tbd         -- update game state
                                , gtime = 1000
                                , pnext = newpiece
                                , hID = 0 -- paused
                                , blstate = (TBState 4 (-1) 0 pSquare) } ,
                           False )                      -- do not restart timer
               else
                 if (nlines > 0)  -- did we make any new lines this time?
                  then do oldNLines <- liftM read $ labelGetText lLn  -- hide some state
                          oldScore  <- liftM read $ labelGetText lSc  -- in the labels
                          let newNLines = oldNLines + nlines          -- tee hee
                          let newScore = oldScore + ((2 * nlines) ^ 3)
                          labelSetText lLn $ show newNLines
                          labelSetText lSc $ show newScore
                          if (newNLines `div` 10) /= (oldNLines `div` 10)
                           then do let newTime = (gtime tgs') `div` 8 * 7 -- speed up!
                                   nHID <- timeoutAdd                -- new timeout
                                           (timerHandler daMain daPrev lLn lSc mtgS)
                                           newTime
                                   timeoutRemove $ hID tgs'          -- remove old timeout
                                   return $ (tgs' { bdstate = tbd    -- update state
                                                  , gtime = newTime
                                                  , pnext = newpiece
                                                  , hID = nHID } ,
                                             False ) -- don't restart the old timeout
                           else return $ ( tgs' { pnext = newpiece   -- else just update
                                                , bdstate = tbd } ,  -- the state
                                           True )                  -- and restart timout
                  else return ( tgs' { pnext = newpiece } , True ) -- no new lines
             modifyMVar_ mtgS (ioify $ (\_ -> tgs''))        -- swap in new game state
             widgetQueueDraw daMain                          -- redraw main display
             widgetQueueDraw daPrev                          -- redraw preview display
             return retval                                   -- done
            else do 
             modifyMVar_ mtgS (ioify $ (\_ -> tgs')) -- just update game state
             widgetQueueDraw daMain                  -- redraw main window
             return True                             -- and reschedule the handler

genRandomPiece :: IO TetrisPiece
genRandomPiece = (\x -> return $ tPieces !! x) =<< randomRIO (0,6)

genNewState :: IO TetrisGameState
genNewState = do
           tp1 <- genRandomPiece
           tp2 <- genRandomPiece
           return $ newGameState tp1 tp2

canvasHandler :: DrawingArea -> ((Int,Int) -> TetrisGameState -> Render ())
                 -> MVar TetrisGameState -> Event -> IO Bool
canvasHandler cname fn st ev = do
           dwin <- widgetGetDrawWindow cname
           csize <- widgetGetSize cname
           cblock <- readMVar st
           renderWithDrawable dwin (fn csize cblock)
           return $ eventSent ev

-- *** MVAR UTILITY FUNCTIONS ***

ioify :: (a -> b) -> (a -> IO b)
ioify y = \x -> return $ y x

modifyThenQueueDraw :: MVar TetrisGameState -> DrawingArea ->
                       (TetrisGameState -> TetrisGameState) -> IO Bool
modifyThenQueueDraw b d fn = modifyMVar_ b (ioify fn) >> widgetQueueDraw d >> return True

-- *** MAIN ***
-- this is the setup before we hand over
-- control to the GTK event loop

main :: IO ()
main = do
  initGUI
-- a whole bunch of GTK stuff
  window <- windowNew
  button <- buttonNewWithMnemonic "  _Quit  "
  hbx    <- hBoxNew False 0
  vbx    <- vBoxNew False 0
  aframe <- aspectFrameNew 0.5 0.5 (Just 0.5)
  afram2 <- aspectFrameNew 0.5 0.5 (Just 0.75)
  canvas <- drawingAreaNew
  preCan <- drawingAreaNew
  lScore <- labelNew $ Just "Score:"
  lLines <- labelNew $ Just "Lines:"
  lLnNum <- labelNew $ Just "0"
  lScNum <- labelNew $ Just "0"

-- game state stuff
  dblock <- newMVar $ newGameState pSquare pSquare

-- setup containers with their contents
  set lLnNum [widgetCanFocus := True]
  set window [containerChild := hbx, containerBorderWidth := 10, windowTitle := "TriHs",
              windowDefaultHeight := 600, windowDefaultWidth := 400]
  set aframe [containerChild := canvas]
  set afram2 [containerChild := preCan]
  windowSetGeometryHints window (Just aframe) (Just (10,10)) (Just (1000,1000))
                                Nothing Nothing (Just (0.8,0.8))
-- hbox
  boxPackStart hbx aframe PackGrow 0
  boxPackEnd hbx vbx PackNatural 0
-- vbox on right
  boxPackStart vbx afram2 PackGrow 0
  boxPackEnd vbx button PackNatural 0
  boxPackEnd vbx lScNum PackNatural 0
  boxPackEnd vbx lScore PackNatural 0
  boxPackEnd vbx lLnNum PackNatural 0
  boxPackEnd vbx lLines PackNatural 0

-- set background color
  widgetModifyBg canvas StateNormal (Color 0 0 0)
  widgetModifyBg preCan StateNormal (Color 0 0 0)
  widgetGrabFocus lLnNum  -- make sure space bar doesn't cause us to quit
  widgetShowAll window

-- handlers for the main and preview windows
  onExpose canvas $ canvasHandler canvas reDraw dblock
  onExpose preCan $ canvasHandler preCan preDraw dblock

-- quit button
  onClicked button (widgetDestroy window)
  onDestroy window mainQuit

-- timer handler
  let tHandler = timerHandler canvas preCan lLnNum lScNum dblock
  startNewGame dblock lLnNum lScNum window tHandler
-- keyboard handler
  onKeyPress window $ handleButtonPress window canvas dblock tHandler lLnNum lScNum

  mainGUI

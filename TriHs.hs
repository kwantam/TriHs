
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
import Graphics.UI.Gtk hiding (eventModifier, eventKeyName, eventSent)
import Graphics.Rendering.Cairo (Render(..))
import Graphics.UI.Gtk.Gdk.Events
import Control.Concurrent.MVar
import Control.Monad (liftM,mapM)
import System.Random
import TriHsPieces
import TriHsDrawing

-- *** HANDLER FUNCTIONS ***
-- these functions are the GTK handlers and their various helpers

handleButtonPress :: Window -> [MVar TetrisGameState] -> [DrawingArea] ->
                     [IO Bool] -> MVar Bool -> Event -> IO Bool
handleButtonPress win (mS1:mS2:[]) (dA1:pA1:dAs) (tH1:tHs) doShad ev = do
      g1State <- readMVar $ mS1
      g2State <- readMVar $ mS2
      let is2p = g2State /= NaS
      let tH2 = head tHs
      let dA2 = head dAs
      let pA2 = head $ tail dAs
      let mqd1 = modifyThenQueueDraw mS1 dA1
      let mqp1 = modifyThenQueueDraw mS1 pA1
      let mqd2 = modifyThenQueueDraw mS2 dA2
      let mqp2 = modifyThenQueueDraw mS2 pA2
      let hID1 = hID g1State
      let hID2 = if is2p then hID g2State else 0
      hBx <- ioify head =<< containerGetChildren win
      case (eventKeyChar ev,eventModifier ev,eventKeyName ev,hID1==0,hID2==0) of
           -- player 1 controls
           (Just 'e',[],_,False,_) -> mqd1 gStateRotateCW
           (Just 'w',[],_,False,_) -> mqd1 gStateRotateCCW
           (Just 'a',[],_,False,_) -> mqd1 gStateMoveLeft
           (Just 'd',[],_,False,_) -> mqd1 gStateMoveRight
           (Just 's',[],_,False,_) -> tH1 >> return True
           (Just 'q',[],_,False,_) -> mqd1 gStateMoveBottom >> tH1 >> return True
           (Just 'f',[],_,False,False) -> genRandomPiece >>= \npiece ->
                                          mqp2 $ gStateReplaceNxt npiece
           -- player 2 controls
           (_,[],"XF86Forward",_,False) -> mqd2 gStateRotateCW
           (_,[],"Up"         ,_,False) -> mqd2 gStateRotateCCW
           (_,[],"Left"       ,_,False) -> mqd2 gStateMoveLeft
           (_,[],"Right"      ,_,False) -> mqd2 gStateMoveRight
           (_,[],"Down"       ,_,False) -> tH2 >> return True
           (_,[],"XF86Back"   ,_,False) -> mqd2 gStateMoveBottom >> tH2 >> return True
           (Just '/',[],_     ,False,False) -> genRandomPiece >>= \npiece ->
                                               mqp1 $ gStateReplaceNxt npiece
           (Just '=', [],_,_,False)  -> mqd2 gStateRotateCW
           (Just '0', [],_,_,False)  -> mqd2 gStateRotateCCW
           (Just 'p', [],_,_,False)  -> mqd2 gStateMoveLeft
           (Just ']', [],_,_,False)  -> mqd2 gStateMoveRight
           (Just '[', [],_,_,False)  -> tH2 >> return True
           (Just '-', [],_,_,False)  -> mqd2 gStateMoveBottom >> tH2 >> return True
           (Just 'o', [],_,False,False) -> genRandomPiece >>= \npiece ->
                                           mqp1 $ gStateReplaceNxt npiece
           -- game controls
           (Just 'p',[Control],_,_,_) -> togglePauseAllGames [(mS1,tH1),(mS2,tH2)] >> return True
           (Just '1',[Control],_,_,_) -> widgetDestroy hBx >>
                                         setupPlayers win hID1 hID2 False >> return True
           (Just '2',[Control],_,_,_) -> widgetDestroy hBx >>
                                         setupPlayers win hID1 hID2 True >> return True
           (Just 'q',[Control],_,_,_) -> widgetDestroy win >> return True
           (Just 's',[Control],_,_,_) -> modifyMVar_ doShad (ioify not) >> 
                                         widgetQueueDraw win >> return True
           -- otherwise don't handle this press
           (_,_,_,_,_) -> return False

-- pause one or two games
togglePauseAllGames :: [(MVar TetrisGameState,IO Bool)] -> IO [Bool]
togglePauseAllGames = mapM toggleIfNotNaS
  where toggleIfNotNaS (mS,tH) = readMVar mS >>= \x -> 
                                 if x /= NaS
                                  then togglePauseGame mS tH
                                  else return False

-- pause game
togglePauseGame :: MVar TetrisGameState -> IO Bool -> IO Bool
togglePauseGame tgS tHnd = do
           gst <- readMVar tgS
           if hID gst == 0
            then do nHID <- timeoutAdd tHnd $ gtime gst
                    modifyMVar_ tgS (ioify $ \_ -> gst { hID = nHID })
                    return True
            else do safeTimeoutRemove $ hID gst
                    modifyMVar_ tgS (ioify $ \_ -> gst { hID = 0 })
                    return True

-- start a new game
startNewGame :: MVar TetrisGameState -> Label -> Label -> Window -> IO Bool -> IO Bool
startNewGame tgS lLn lSn w tHnd = do
           gst <- readMVar tgS
           safeTimeoutRemove $ hID gst           -- remove old timeout; OK if this fails
           nGS <- genNewState gst                -- new gamestate
           nHID <- timeoutAdd tHnd 1000          -- add new timeout
           modifyMVar_ tgS (ioify $ \_ -> nGS { hID = nHID }) -- save new state
           labelSetText lLn "0"                  -- reset lines count
           labelSetText lSn "0"                  -- reset score
           widgetQueueDraw w                     -- redraw screen
           return True

-- oof this is so ugly and imperative
-- this code makes me feel a little dirty
timerHandler :: MVar TetrisGameState -> MVar TetrisGameState -> IO Bool
timerHandler mtgS mtgS2 = do
           tgs <- readMVar mtgS                     -- read game state
           tgs2 <- readMVar mtgS2
           let daMain = can tgs
           let daPrev = pcan tgs
           let lLn = lLNum tgs
           let lSc = lScore tgs
           let (collided,tgs') = stateDropPiece tgs -- drop the piece, detect collisions, &c
           if collided  -- if we got a collision, we have some splainin to do
            then do 
             newpiece <- genRandomPiece                            -- first, next piece
             let (nlines,tbd) = removeCompleteLines $ bdstate tgs' -- any complete lines?
             (tgs'',tgs2',retval) <-                               -- OK, now this is ugly
              if stateAboveBounds $ blstate tgs -- if the piece that collided is off the top
               then do                  -- game over!
                 safeTimeoutRemove $ hID tgs'           -- cancel interrupt
                 return $ ( tgs' { bdstate = tbd        -- update game state
                                 , gtime = 1000
                                 , pnext = newpiece
                                 , hID = 0 -- paused
                                 , blstate = (TBState 4 (-1) 0 pSquare) }
                          , tgs2                        -- no update to 2p state
                          , False )                     -- do not restart timer
               else
                 if (nlines > 0)  -- did we make any new lines this time?
                  then do oldNLines <- liftM read $ labelGetText lLn  -- hide some state
                          oldScore  <- liftM read $ labelGetText lSc  -- in the labels
                          let newNLines = oldNLines + nlines          -- tee hee
                          let newScore = oldScore + ((2 * nlines) ^ 3)
                          labelSetText lLn $ show newNLines
                          labelSetText lSc $ show newScore
                          -- increase the speed after every 10 lines
                          -- if it's two-player, your lines increase the opponent's speed
                          case ((newNLines `div` 10) /= (oldNLines `div` 10),tgs2) of
                            (True,NaS) -> do    -- 1 player game
                                   let newTime = (gtime tgs') `div` 6 * 5 -- speed up!
                                   nHID <- timeoutAdd                 -- new timeout
                                           (timerHandler mtgS mtgS2)
                                           newTime
                                   safeTimeoutRemove $ hID tgs'       -- remove old timeout
                                   return $ ( tgs' { bdstate = tbd    -- update state
                                                   , gtime = newTime
                                                   , pnext = newpiece
                                                   , hID = nHID }
                                            , tgs2                    -- no update to 2p state
                                            , False ) -- don't restart the old timeout
                            (True, _ ) -> do    -- 2 player game
                                   let newTime = (gtime tgs2) `div` 6 * 5 -- speed up other player
                                   nHID <- timeoutAdd                     -- add new timeout for other player
                                           (timerHandler mtgS2 mtgS)
                                           newTime
                                   safeTimeoutRemove $ hID tgs2           -- remove old timeout
                                   return $ ( tgs' { bdstate = tbd
                                                   , pnext = newpiece }
                                            , tgs2 { gtime = newTime      -- update other player's time state
                                                   , hID = nHID }
                                            , True )                      -- restart timer
                            (False, _) -> return $ ( tgs' { pnext = newpiece  -- else just update
                                                          , bdstate = tbd }   -- the state
                                                   , tgs2                     -- no update to 2p state
                                                   , True )                   -- and restart timout
                  else return ( tgs' { pnext = newpiece } , tgs2, True ) -- no new lines
             modifyMVar_ mtgS (ioify $ (\_ -> tgs''))        -- swap in new game state
             modifyMVar_ mtgS2 (ioify $ (\_ -> tgs2'))
             widgetQueueDraw daMain                          -- redraw main display
             widgetQueueDraw daPrev                          -- redraw preview display
             return retval                                   -- done
            else do 
             modifyMVar_ mtgS (ioify $ (\_ -> tgs')) -- just update game state
             widgetQueueDraw daMain                  -- redraw main window
             return True                             -- and reschedule the handler

genRandomPiece :: IO TetrisPiece
genRandomPiece = (\x -> return $ tPieces !! x) =<< randomRIO (0,6)

genNewState :: TetrisGameState -> IO TetrisGameState
genNewState tgs = do
           tp1 <- genRandomPiece
           tp2 <- genRandomPiece
           return $ tgs { blstate = (newState tp1)
                        , pnext = tp2 }

canvasHandler :: DrawingArea -> (Bool -> (Int,Int) -> TetrisGameState -> Render ())
                 -> MVar TetrisGameState -> MVar Bool -> Event -> IO Bool
canvasHandler cname fn st shSt ev = do
           dwin <- widgetGetDrawWindow cname
           csize <- widgetGetSize cname
           cblock <- readMVar st
           doShad <- readMVar shSt
           renderWithDrawable dwin (fn doShad csize cblock)
           return $ eventSent ev

safeTimeoutRemove :: HandlerId -> IO ()
safeTimeoutRemove hndId = if hndId /= 0 then timeoutRemove hndId else return ()

-- *** MVAR UTILITY FUNCTIONS ***

ioify :: (a -> b) -> (a -> IO b)
ioify y = \x -> return $ y x

modifyThenQueueDraw :: MVar TetrisGameState -> DrawingArea ->
                       (TetrisGameState -> TetrisGameState) -> IO Bool
modifyThenQueueDraw b d fn = modifyMVar_ b (ioify fn) >> widgetQueueDraw d >> return True

-- *** WINDOW SETUP FUNCTIONS ***

-- this is the 1 player window setup
setupPlayers :: Window -> HandlerId -> HandlerId -> Bool -> IO ()
setupPlayers window t1 t2 is2p = do
-- a whole bunch of GTK stuff
           safeTimeoutRemove t1
           safeTimeoutRemove t2

           button <- buttonNewWithMnemonic "  _Quit  "
           hbx    <- hBoxNew False 0
           vbx    <- vBoxNew False 0
           aframe <- aspectFrameNew 0.5 0.5 (Just 0.5)
           afram2 <- aspectFrameNew 0.5 0.5 (Just 0.75)
           canvas <- drawingAreaNew
           preCan <- drawingAreaNew
           lScore <- labelNew $ Just "p1 Score:"
           lLines <- labelNew $ Just "p1 Lines:"
           lLnNum <- labelNew $ Just "0"
           lScNum <- labelNew $ Just "0"

-- make second set if necessary
           aframe2 <- if is2p then aspectFrameNew 0.5 0.5 (Just 0.5) else return aframe
           afram22 <- if is2p then aspectFrameNew 0.5 0.5 (Just 0.75) else return afram2
           canvas2 <- if is2p then drawingAreaNew else return canvas
           preCan2 <- if is2p then drawingAreaNew else return preCan
           lScore2 <- if is2p then labelNew $ Just "p2 Score:" else return lScore
           lLines2 <- if is2p then labelNew $ Just "p2 Lines:" else return lLines
           lLnNum2 <- if is2p then labelNew $ Just "0" else return lLnNum
           lScNum2 <- if is2p then labelNew $ Just "0" else return lScNum

-- setup containers with their contents
           set lLnNum [widgetCanFocus := True]
           set window [containerChild := hbx, containerBorderWidth := 10, windowTitle := "TriHs",
                       windowDefaultHeight := 600, windowDefaultWidth := 480]
           set aframe [containerChild := canvas]
           set afram2 [containerChild := preCan]
           let aspectRatio = if is2p then 1.4 else 0.75
           windowSetGeometryHints window (Just aframe) (Just (300,300)) (Just (1000,1000))
                                         Nothing Nothing (Just (aspectRatio,aspectRatio))
           if is2p then do set aframe2 [containerChild := canvas2]
                           set afram22 [containerChild := preCan2]
                   else return ()

-- main hbox
           boxPackStart hbx aframe PackGrow 0
           boxPackStart hbx vbx PackNatural 0
           if is2p then boxPackStart hbx aframe2 PackGrow 0 else return ()

-- vbox on right (or middle if 2p game)
           boxPackStart vbx afram2 PackGrow 0
           boxPackStart vbx lLines PackNatural 0
           boxPackStart vbx lLnNum PackNatural 0
           boxPackStart vbx lScore PackNatural 0
           boxPackStart vbx lScNum PackNatural 0
           if is2p
            then do boxPackStart vbx afram22 PackGrow 0
                    boxPackStart vbx lLines2 PackNatural 0
                    boxPackStart vbx lLnNum2 PackNatural 0
                    boxPackStart vbx lScore2 PackNatural 0
                    boxPackStart vbx lScNum2 PackNatural 0
            else return ()
           boxPackStart vbx button PackNatural 0

-- set background color
           let canvases = if is2p then [canvas,preCan,canvas2,preCan2] else [canvas,preCan]
           mapM (\x -> widgetModifyBg x StateNormal (Color 0 0 0)) canvases

-- make sure space bar doesn't cause us to quit
           widgetGrabFocus lLnNum  
           widgetShowAll window

-- game state stuff
           mg1State <- newMVar $ newGameState pSquare pSquare canvas preCan lLnNum lScNum
           mg2State <- if is2p 
                        then newMVar $ newGameState pSquare pSquare canvas2 preCan2 lLnNum2 lScNum2
                        else newMVar NaS
           shadState <- newMVar False


-- handlers for the main and preview windows
           let exposeStuff = zip3 canvases 
                                  [mg1State,mg1State,mg2State,mg2State] $
                                  cycle [reDraw,preDraw]
           mapM (\(c,s,d) -> onExpose c $ canvasHandler c d s shadState) exposeStuff

-- quit button
           onClicked button (widgetDestroy window)
           onDestroy window mainQuit

-- timer handler
           let tH1 = timerHandler mg1State mg2State
           let tH2 = if is2p then timerHandler mg2State mg1State
                             else return False
           startNewGame mg1State lLnNum lScNum window tH1
           if is2p then startNewGame mg2State lLnNum2 lScNum2 window tH2 else return False

-- keyboard handler
           let handlers = if is2p then [tH1,tH2] else [tH1]
           kHandler <- onKeyPress window $
                       handleButtonPress window [mg1State,mg2State] canvases handlers shadState
           onDestroy hbx $ signalDisconnect kHandler
           return ()

-- *** MAIN ***
-- kick off 1-player game
main :: IO ()
main = do
           initGUI
           window <- windowNew
           setupPlayers window 0 0 False
           mainGUI

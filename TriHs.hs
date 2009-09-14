
module Main where

import Data.Array as DA
import Graphics.UI.Gtk hiding (fill)
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk.Gdk.Events
import Control.Concurrent.MVar
import Control.Monad (liftM,mapM)
import System.Random
import TriHsPieces

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

setSourceTBlock :: TetrisBlock -> Render ()
setSourceTBlock t = setSourceRGB tr tg tb
  where (tr,tg,tb) = tBlockToRGBd t

-- *** HANDLER FUNCTIONS ***
-- these functions are the GTK handlers and their various helpers

handleButtonPress :: Window -> DrawingArea -> MVar TetrisGameState ->
                     IO Bool -> Label -> Label -> Event -> IO Bool
handleButtonPress w d b tHnd lLn lSn ev = do
      let mqd = modifyThenQueueDraw b d
      case (eventKeyChar ev,eventModifier ev,eventKeyName ev) of
           (_ ,[],   "Up") -> mqd gStateRotateCCW
           (_ ,[], "Down") -> mqd gStateMoveDown
           (_ ,[], "Left") -> mqd gStateMoveLeft
           (_ ,[],"Right") -> mqd gStateMoveRight
           --(Just 'e',[],_) -> mqd gStateRotateCW
           --(Just 'w',[],_) -> mqd gStateRotateCCW
           --(Just 'r',[],_) -> (\x -> mqd (gameStateLift $ stateReplaceTP x))
           --                    =<< genRandomPiece
           (Just 'r',[Control],_) -> do gst <- readMVar b
                                        let tHID = hID gst
                                        timeoutRemove tHID
                                        nGS <- genNewState
                                        nHID <- timeoutAdd tHnd 1000
                                        let nGS' = gStateReplaceHID nHID nGS
                                        modifyMVar_ b (ioify $ \_ -> nGS')
                                        labelSetText lLn "0"
                                        labelSetText lSn "0"
                                        widgetQueueDraw w
                                        return True
           (Just 'q',[Control],_) -> widgetDestroy w >> return True
           (_       ,_ ,_) -> return False

reDraw :: (Int,Int) -> TetrisGameState -> Render ()
reDraw (x,y) tgS = do
  let (TBState cx cy rot tp) = blstate tgS
  let bstate = bdstate tgS
  let ddx = fromIntegral x / 10
  let ddy = fromIntegral y / 20
  drawTetrisBoard ddx ddy bstate
  drawTetrisPiece tp ddx ddy cx cy rot

preDraw :: (Int,Int) -> TetrisGameState -> Render ()
preDraw (x,y) tgS = do
  let np = pnext tgS
  let ddx = fromIntegral x / 3
  let ddy = fromIntegral y / 4
  drawTetrisPiece np ddx ddy 1 3 0

-- oof this is so ugly and imperative
timerHandler :: DrawingArea -> DrawingArea -> Label -> Label -> MVar TetrisGameState -> IO Bool
timerHandler daMain daPrev lLn lSc mtgS = do
           tgs <- readMVar mtgS
           let (collided,tgs') = stateDropPiece tgs -- drop the piece, detect collisions, &c
           if collided
            then do 
             newpiece <- genRandomPiece
             let (nlines,tbd) = removeCompleteLines $ bdstate tgs'
             (tgs'',retval) <-
                    if (nlines > 0)
                     then do oldNLines <- liftM read $ labelGetText lLn
                             oldScore  <- liftM read $ labelGetText lSc
                             let newNLines = oldNLines + nlines
                             let newScore = oldScore + (nlines ^ 2)
                             labelSetText lLn $ show newNLines
                             labelSetText lSc $ show newScore
                             if newNLines `div` 10 /= oldNLines `div` 10
                              then do let newTime = (gtime tgs') `div` 8 * 7
                                      timeoutRemove $ hID tgs'
                                      nHID <- timeoutAdd 
                                              (timerHandler daMain daPrev lLn lSc mtgS)
                                              newTime
                                      return $ (tgs' { bdstate = tbd
                                                     , gtime = newTime
                                                     , pnext = newpiece
                                                     , hID = nHID } ,
                                                False )
                              else return $ ( tgs' { pnext = newpiece 
                                                   , bdstate = tbd } ,
                                              True )
                     else return ( tgs' { pnext = newpiece } , True )
             modifyMVar_ mtgS (ioify $ (\_ -> tgs''))
             widgetQueueDraw daMain
             widgetQueueDraw daPrev
             return retval
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

-- *** STATE MODIFICATION FUNCTIONS ***
-- these functions are for working with MVars

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

  nGS <- genNewState
  dblock <- newMVar nGS

  set window [containerChild := hbx, containerBorderWidth := 10, windowTitle := "TriHs",
              windowDefaultHeight := 600, windowDefaultWidth := 400]
  set aframe [containerChild := canvas]
  set afram2 [containerChild := preCan]
  windowSetGeometryHints window (Just aframe) (Just (10,10)) (Just (1000,1000))
                                Nothing Nothing (Just (0.8,0.8))

  boxPackStart hbx aframe PackGrow 0
  boxPackEnd hbx vbx PackNatural 0

  boxPackStart vbx afram2 PackGrow 0
  boxPackEnd vbx button PackNatural 0
  boxPackEnd vbx lScNum PackNatural 0
  boxPackEnd vbx lScore PackNatural 0
  boxPackEnd vbx lLnNum PackNatural 0
  boxPackEnd vbx lLines PackNatural 0

  widgetModifyBg canvas StateNormal (Color 0 0 0)
  widgetModifyBg preCan StateNormal (Color 0 0 0)
  widgetShowAll window

  onExpose canvas $ canvasHandler canvas reDraw dblock
  onExpose preCan $ canvasHandler preCan preDraw dblock

  onClicked button (widgetDestroy window)
  onDestroy window mainQuit

  let tHandler = timerHandler canvas preCan lLnNum lScNum dblock
  tHandlID <- timeoutAdd tHandler 1000
  modifyMVar_ dblock (ioify $ gStateReplaceHID tHandlID)
  onKeyPress window $ handleButtonPress window canvas dblock tHandler lLnNum lScNum

  mainGUI

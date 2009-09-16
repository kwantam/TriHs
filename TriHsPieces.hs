
{- Copyright (C) 2009 Riad S Wahby <rsw@jfet.org>
 - 
-- This file is part of TriHs
--
--  TriHs is free software.  It comes without any warranty, to
--  to the extent permitted by applicable law.  You can redistribute it
--  and/or modify it under the terms of the Do What The Fuck You Want To
--  Public License, Version 2, as published by Sam Hocevar.  See
--  the COPYING file or http://sam.zoy.org/wtfpl/COPYING for more details
 -
 -}

module TriHsPieces where

import Data.Array as DA
import Data.List as DL
import System.Glib.MainLoop (HandlerId(..))

-- *** DATA TYPES ***

type Coord = (Int,Int)
type TransCoord = (Int,Int)

type TetrisLine = DA.Array Int TetrisBlock
type TetrisBoard = DA.Array Int TetrisLine

data TetrisPiece = TPiece
      { segCoords :: [Coord]      -- coords of the boxes
      , rotTrans  :: [TransCoord] -- translation to apply before rotating
      , block     :: TetrisBlock  -- color of the blocks
      } deriving (Eq, Show)

data TetrisBlock = Nil | Red | Green | Blue | Yellow | Cyan | Purple | Orange
  deriving (Eq, Show)

data TetrisRotation = TRot
      { kx :: Int                 -- X factor when rotating
      , ky :: Int                 -- Y factor when rotating
      , sw :: Bool                -- swap X and Y?
      } deriving (Eq, Show)

data TetrisBlockState = TBState
      { cx :: Int                 -- x position
      , cy :: Int                 -- y position
      , rot :: Int                -- rotation
      , tp :: TetrisPiece         -- which piece
      } deriving (Eq, Show)

data TetrisGameState = TGState
      { blstate :: TetrisBlockState
      , bdstate :: TetrisBoard
      , gtime :: Int
      , pnext :: TetrisPiece
      , hID :: HandlerId
      } deriving (Eq, Show)

-- *** TETRIS PIECES ***

pLine   = TPiece [(0,0),(0,-1),(0,-2),(0,-3)]   [(0,0),(-2,0),(0,-3),(1,0)]   Cyan    -- I
pLRight = TPiece [(0,0),(-1,0),(0,-1),(0,-2)]   [(0,0),(-1,0),(-1,-2),(1,-1)] Blue    -- J
pLLeft  = TPiece [(0,0),(1,0),(0,-1),(0,-2)]    [(0,0),(-1,-1),(0,-2),(1,0)]  Orange  -- L
pSquare = TPiece [(0,0),(1,0),(1,-1),(0,-1)]    [(0,0),(0,-1),(1,-1),(1,0)]   Yellow  -- O
pZLeft  = TPiece [(0,0),(0,-1),(-1,-1),(-1,-2)] [(0,0),(-1,0),(-1,-2),(1,-1)] Green   -- S
pTee    = TPiece [(0,0),(0,-1),(0,-2),(1,-1)]   [(0,0),(-1,-1),(0,-2),(1,0)]  Purple  -- T
pZRight = TPiece [(0,0),(0,-1),(1,-1),(1,-2)]   [(0,0),(-1,-1),(1,-2),(1,0)]  Red     -- Z

tPieces = [pSquare,pLLeft,pLRight,pZLeft,pZRight,pLine,pTee]

-- *** COLOR DEFINITIONS *** 

tBlockToRGBd :: TetrisBlock -> (Double,Double,Double)
tBlockToRGBd t = case t of
                     Nil -> (0,0,0)
                     Red -> (1,0,0)
                     Green -> (0,1,0)
                     Blue -> (0,0,1)
                     Yellow -> (1,1,0)
                     Cyan -> (0,1,1)
                     Purple -> (0.75,0,0.75)
                     Orange -> (1,0.5,0)

-- *** ROTATIONS ***

tRotations :: [TetrisRotation]
tRotations = [ TRot ( 1) ( 1) False
             , TRot ( 1) (-1) True
             , TRot (-1) (-1) False
             , TRot (-1) ( 1) True
             ]

-- *** STATE STUFF ***
-- OK, we break the abstraction in a few places in Main
-- because replacing stuff one by one would be fucking clunky

newState :: TetrisPiece -> TetrisBlockState
newState tp = TBState 4 0 0 tp

stateReplaceTP :: TetrisPiece -> TetrisBlockState -> TetrisBlockState
stateReplaceTP tp (TBState cx cy r _) = TBState cx cy r tp

gStateReplaceGtm :: Int -> TetrisGameState -> TetrisGameState
gStateReplaceGtm gtm tgs = tgs { gtime = gtm }

gStateReplaceNxt :: TetrisPiece -> TetrisGameState -> TetrisGameState
gStateReplaceNxt tpc tgs = tgs { pnext = tpc }

gStateReplaceHID :: HandlerId -> TetrisGameState -> TetrisGameState
gStateReplaceHID hid tgs = tgs { hID = hid }

newGameState :: TetrisPiece -> TetrisPiece -> TetrisGameState
newGameState tp1 tp2 = TGState (newState tp1) emptyBoard 1000 tp2 0

-- lift a BlockState operation onto a GameState
gameStateLift :: (TetrisBlockState -> TetrisBlockState) -> TetrisGameState -> TetrisGameState
gameStateLift fn tgst = tgst { blstate = fn $ blstate tgst }

stateModify :: Int -> Int -> Int -> TetrisBlockState -> TetrisBlockState
stateModify dx dy dr (TBState cx cy rot tp) = TBState (cx+dx) (cy+dy) (mod (rot+dr) 4) tp

-- unchecked moves
stateMoveRight_ = stateModify ( 1) ( 0) ( 0)
stateMoveLeft_  = stateModify (-1) ( 0) ( 0)
stateMoveDown_  = stateModify ( 0) ( 1) ( 0)
stateMoveUp_    = stateModify ( 0) (-1) ( 0)
stateRotateCW_  = stateModify ( 0) ( 0) ( 1)
stateRotateCCW_ = stateModify ( 0) ( 0) (-1)

-- generalized move, but check for out of bounds
bCheckedMove :: (TetrisBlockState -> TetrisBlockState) -> TetrisBlockState -> TetrisBlockState
bCheckedMove fn oldB = if stateOffGrid newB then oldB else newB
  where newB = fn oldB

-- checked moves (side and bottom borders only)
stateMoveRight  = bCheckedMove stateMoveRight_
stateMoveLeft   = bCheckedMove stateMoveLeft_
stateMoveDown   = bCheckedMove stateMoveDown_
stateMoveUp     = bCheckedMove stateMoveUp_
stateRotateCW   = bCheckedMove stateRotateCW_
stateRotateCCW  = bCheckedMove stateRotateCCW_

-- check for out of bounds and collision -- this requires the whole game state
checkedMove :: (TetrisBlockState -> TetrisBlockState) -> TetrisGameState -> TetrisGameState
checkedMove fn oldS = if collided then oldS else newS
  where newB = fn $ blstate oldS
        newS = oldS { blstate = newB }
        collided = stateIsCollisionOrOffGrid (bdstate oldS) newB

-- checked moves (borders plus collisions)
gStateMoveRight  = checkedMove stateMoveRight_
gStateMoveLeft   = checkedMove stateMoveLeft_
gStateMoveDown   = checkedMove stateMoveDown_
gStateMoveUp     = checkedMove stateMoveUp_
gStateRotateCW   = checkedMove stateRotateCW_
gStateRotateCCW  = checkedMove stateRotateCCW_

-- drop a block as far as it can go by repeatedly dropping it until it collides
gStateMoveBottom :: TetrisGameState -> TetrisGameState
gStateMoveBottom tgS = snd.head $ dropWhile fst $ 
                       DL.iterate (gApplyCompare gStateMoveDown) (True,tgS)
  where gApplyCompare fn (_, tgS) = (tgS /= tgS', tgS')
          where tgS' = fn tgS

-- *** FUNCTIONS TO MANIPULATE BLOCKS AND STATES ***

applyRotation :: TetrisRotation -> TransCoord -> Coord -> Coord
applyRotation (TRot kx ky sw) (dx,dy) (x,y) = if sw then (y'+dx,x'+dy) else (x'+dx,y'+dy)
  where x' = kx*x
        y' = ky*y

shiftRotateTetrisPiece :: Coord -> Int -> TetrisPiece -> TetrisPiece
shiftRotateTetrisPiece (dx,dy) rot (TPiece sC rT b) = TPiece sC' [] b
  where (dtx,dty) = rT !! rot
        rotTetr = tRotations !! rot
        sC' = map (applyRotation rotTetr (dx+dtx,dy+dty)) sC

-- take a state and apply the shift and rotation to the piece
tetrisBlockStateToPiece :: TetrisBlockState -> TetrisPiece
tetrisBlockStateToPiece (TBState cx cy rot tp) =
        shiftRotateTetrisPiece (cx,cy) rot tp

-- *** FUNCTIONS TO MODIFY BOARD ***
-- these functions modify the TetrisBoard,
-- either by removing lines or by adding
-- a piece or a state to an existing board
-- placed pieces are represented by a two-dimensional array of blocks

emptyLine :: TetrisLine
emptyLine = DA.listArray (0,9) $ take 10 (repeat Nil)

emptyBoard :: TetrisBoard
emptyBoard = DA.listArray (0,19) $ take 20 (repeat emptyLine)

lineIsComplete :: TetrisLine -> Bool
lineIsComplete ln = all (/= Nil) $ DA.elems ln

completeLines :: TetrisBoard -> [Int]
completeLines tboard = map fst $ filter (lineIsComplete.snd) $ DA.assocs tboard

removeLine :: Int -> TetrisBoard -> TetrisBoard
removeLine lnum = (// [(0,emptyLine)]).(ixmap (0,19) nline)
  where nline i = case (i<=lnum,i) of
                   (_,   0) -> 0
                   (True,_) -> i-1
                   (_   ,_) -> i

-- removeLinesSorted requires a list of lines in reverse sorted order!
removeLinesSorted :: [Int] -> TetrisBoard -> TetrisBoard
removeLinesSorted lnums tboard = foldr removeLine tboard lnums

removeLines :: [Int] -> TetrisBoard -> TetrisBoard
removeLines lnums tboard = removeLinesSorted (reverse $ sort lnums) tboard

removeCompleteLines :: TetrisBoard -> (Int,TetrisBoard)
removeCompleteLines tboard = (nlines,tboard')
  where rlines = reverse $ completeLines tboard
        nlines = length rlines
        tboard' = removeLinesSorted rlines tboard

insertBlock :: TetrisBlock -> Coord -> TetrisBoard -> TetrisBoard
insertBlock tb (x,y) tboard = tboard // [(y,newX)]
  where oldX = tboard ! y
        newX = oldX // [(x,tb)]

addPieceToBoard :: TetrisBoard -> TetrisPiece -> TetrisBoard
addPieceToBoard tboard (TPiece sC _ b) = foldr (insertBlock b) tboard $ 
                                               filter (not.outOfBounds_) sC

addStateToBoard :: TetrisBoard -> TetrisBlockState -> TetrisBoard
addStateToBoard tbd tbs = addPieceToBoard tbd $ tetrisBlockStateToPiece tbs

-- *** COLLISION DETECTION ***
-- these predicates are used to detect
-- collisions with some combination of bottom,
-- board, and sides

pieceOffBottom :: TetrisPiece -> Bool
pieceOffBottom (TPiece sC _ _) = any outOfBottom sC

stateOffBottom :: TetrisBlockState -> Bool
stateOffBottom = pieceOffBottom.tetrisBlockStateToPiece

pieceOffGrid :: TetrisPiece -> Bool
pieceOffGrid (TPiece sC _ _) = any outOfBounds sC

stateOffGrid :: TetrisBlockState -> Bool
stateOffGrid = pieceOffGrid.tetrisBlockStateToPiece

-- is the piece below the bottom or off either side?
outOfBounds :: Coord -> Bool
outOfBounds (x,y) = x < 0 || x > 9 || y > 19

-- outOfBounds, including off the top of the board
outOfBounds_ :: Coord -> Bool
outOfBounds_ (x,y) = x < 0 || x > 9 || y < 0 || y > 19

-- off the bottom of the board
outOfBottom :: Coord -> Bool
outOfBottom (x,y) = y > 19

lookupCoord :: TetrisBoard -> Coord -> TetrisBlock
lookupCoord tboard (x,y) = (tboard ! y) ! x

isCollision :: TetrisBoard -> TetrisPiece -> Bool
isCollision tboard (TPiece sC _ _) = 
            any ((/=Nil).(lookupCoord tboard)) $ filter (not.outOfBounds_) sC

isCollisionOrBottom :: TetrisBoard -> TetrisPiece -> Bool
isCollisionOrBottom tboard tpiece = isCollision tboard tpiece || pieceOffBottom tpiece

isCollisionOrOffGrid :: TetrisBoard -> TetrisPiece -> Bool
isCollisionOrOffGrid tboard tpiece = isCollision tboard tpiece || pieceOffGrid tpiece

liftStatePred :: (TetrisBoard -> TetrisPiece -> Bool) -> TetrisBoard -> TetrisBlockState -> Bool
liftStatePred pred tbd tbs = pred tbd $ tetrisBlockStateToPiece tbs

stateIsCollision = liftStatePred isCollision
stateIsCollisionOrBottom = liftStatePred isCollisionOrBottom
stateIsCollisionOrOffGrid = liftStatePred isCollisionOrOffGrid

-- this is the function we use when the timer advances one click
stateDropPiece :: TetrisGameState -> (Bool,TetrisGameState)
stateDropPiece tgs = (collided,tgs')
  where tbs = blstate tgs
        tbd = bdstate tgs
        tbs' = stateMoveDown_ tbs
        collided = stateIsCollisionOrBottom tbd tbs'
        tbd' = if collided then addStateToBoard tbd tbs else tbd
        tbs'' = if collided then newState $ pnext tgs else tbs'
        tgs' = tgs { blstate = tbs'' , bdstate = tbd' }

-- *** TEST STUFF ***
-- stuff for testing board handling

redLine :: TetrisLine
redLine = DA.listArray (0,9) $ take 10 (repeat Red)

blueLine :: TetrisLine
blueLine = DA.listArray (0,9) $ take 10 (repeat Blue)

testBoard :: TetrisBoard
testBoard = DA.listArray (0,19) $ take 18 (repeat emptyLine) ++ [redLine,blueLine]

-- This software is copyright (c) 1996-2005 by
--      John Tromp
--      Insulindeweg 908
--      1095 DX Amsterdam
--      Netherlands
-- E-mail: john.tromp at gmail.com
--
-- This notice must not be removed.
-- This software must not be sold for profit.
-- You may redistribute if your distributees have the
-- same rights and restrictions.

module Main where

import Data.Bits
import Data.Char
import Data.Word
import Data.Array.MArray
import Data.Array.IO
import Data.IORef
import Numeric
import System.CPUTime
import System.IO

import Connect4(width,height,Game(Game),listGame,isWonGame,isWinnable,goodMoves,size,bsize,moveEval,encode,specialGame,move)
import GameTreeSearch(Hash(Hash),GameTree(GTLoss,GTDraw,GTBranch),alphabeta,loss,losswin,win,newTT,TTable,statsTT,intlog,getPosed,ratio,locksize)

ttsize = 8306069 -- should be at least 2^bsize-locksize

makeGameTree game@(Game n _ _ _) h0 h1 =
  if n==size-1 then GTDraw else -- assume last move doesn't win
  if null children then GTLoss else
  if n==size-2 then GTDraw -- two moves left without opponent win is a draw
  else GTBranch (Hash lock hash) h0 children where
    key = encode game
    lock = fromIntegral (key `shiftR` (bsize - locksize))
    hash = fromIntegral (key `mod` (fromIntegral ttsize))
    children = [(h,makeGameTree g h1 h0) | (h,g) <- goodMoves game]

solve theGame = do
  --let moves = map ((\i->i-1).digitToInt) $ filter isDigit line
  let game@(Game n _ _ _) = theGame--listGame moves
  if (isWonGame game) then putStrLn "already lost" else
   if isWinnable game then putStrLn "instant win" else do
  tt <- newTT ttsize :: IO TTable
  hist0 <- thaw moveEval :: IO (IOUArray Int Int)
  hist1 <- thaw moveEval :: IO (IOUArray Int Int)
  --putStrLn$("\nSolving "++).shows n.(("-ply position after "++line)++)$" . . ."
  --putStrLn "Solving game"
  nodes <- newIORef 0 :: IO (IORef Word64)
  --tstart <- getCPUTime
  score <- alphabeta nodes tt losswin (makeGameTree game hist0 hist1)
  --tend <- getCPUTime
  posed <- getPosed tt
  n <- readIORef nodes
  putStrLn $ ("score = "++).shows score.(" ("++).(("-<=>+"!!(score-loss)):).
     (")  work = "++) $ show $ intlog posed
  --let msecs = (tend-tstart) `div` 1000000000
  --putStrLn $ shows n . (" pos / "++) . shows msecs .
  --   (" msec = "++) . showFFloat (Just 1) (ratio n msecs) $ " Kpos/sec"
  stats <- statsTT tt
  putStrLn stats

pass (Game n tm ntm heights) = (Game (n+1) ntm tm heights)

makeMove col game = snd $ move game $ col-1

startGame = listGame $ map (\x -> x-1) [3,4,4,3,3,3,7,10,7,10,7,7,3,3,10]

theGame = startGame

main = do
  putStrLn (show startGame)
  putStrLn $ ("Fhourstones 3.1 (Haskell)\nBoardsize = "++) .
           shows width . ('x':) . shows height . ("\nUsing "++) .
           shows ttsize $ " transposition table entries."
  --input <- getContents
  --mapM_ solve $ lines input
  putStrLn "The current position is"
  putStrLn (show theGame)
  solve theGame
  putStrLn "If we pass..."
  putStrLn (show $ pass theGame)
  solve $ pass theGame
  putStrLn "Column 1"
  putStrLn (show $ makeMove 1 theGame)
  solve $ makeMove 1 theGame
  putStrLn "Column 2"
  putStrLn (show $ makeMove 2 theGame)
  solve $ makeMove 2 theGame
  putStrLn "Column 3"
  putStrLn (show $ makeMove 3 theGame)
  solve $ makeMove 3 theGame
  putStrLn "Column 4"
  putStrLn (show $ makeMove 4 theGame)
  solve $ makeMove 4 theGame
  putStrLn "Column 5"
  putStrLn (show $ makeMove 5 theGame)
  solve $ makeMove 5 theGame
  putStrLn "Column 6"
  putStrLn (show $ makeMove 6 theGame)
  solve $ makeMove 6 theGame
  putStrLn "Column 7"
  putStrLn (show $ makeMove 7 theGame)
  solve $ makeMove 7 theGame

{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main where

import qualified Data.Map as Map
import Data.Maybe
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Interface.IO.Interact
import Solver
import Verlet

render :: World -> IO Picture
render (w, _) = return $ pictures $ fmap renderObj (map snd (Map.toList w))

renderObj :: Verlet -> Picture
renderObj v = uncurry translate (pos v) $ circleSolid (r v)

spawnVerlet :: Float -> Float -> Verlet
spawnVerlet x y = Verlet {pos = (x, y), prev_pos = (x, y), a = (0, 0), r = 10.0}

handleEvent :: Event -> World -> IO World
handleEvent (EventKey (MouseButton LeftButton) Down _ (xPos, yPos)) (w, g)
  | not (null w) =
      let (xPos', yPos') = normalizePos (xPos, yPos)
          gridID = (xPos' `div` cellSize, yPos' `div` cellSize)
          vals = fromMaybe [] (Map.lookup gridID g)
       in do
            print "In not null"
            print g
            return (Map.insert (maxKey + 1) verlet w, Map.insert gridID (vals ++ [maxKey + 1]) g)
  | otherwise =
      let (xPos', yPos') = normalizePos (xPos, yPos)
          gridID = (xPos' `div` cellSize, yPos' `div` cellSize)
          g' = Map.insert gridID [0] g
       in do
            print "In null"
            print ((null w))
            print g'
            print w
            return (Map.singleton 0 verlet, g')
  where
    maxKey = maximum $ map fst (Map.toList w)
    verlet = spawnVerlet xPos yPos
handleEvent _ w = return w

main :: IO ()
main =
  playIO
    (InWindow "Nice Window" (1000, 1000) (0, 0))
    white
    60
    (Map.empty, Map.empty)
    render
    handleEvent
    update

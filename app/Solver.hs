{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Solver where

import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Graphics.Gloss
import Verlet

type World = (Map Int Verlet, Grid)

type Grid = Map (Int, Int) [Int]

width :: Float
width = 200

height :: Float
height = 200

cellSize :: Int
cellSize = 10

grid :: [(Int, Int)]
grid = pairs' [0 .. round width `div` cellSize]

update :: Float -> World -> IO World
update dt w@(m, g) = do
  print g
  return $ updatePositions dt $ solveCollisions (applyConstraint $ applyGravity w) (pairs w)

updatePositions :: Float -> World -> World
updatePositions dt (m, g) = (Map.map (updatePosition dt) m, g)

applyGravity :: World -> World
applyGravity (m, g) = (Map.map (accelerate (0, -500)) m, g)

applyConstraint :: World -> World
applyConstraint (m, g) = (Map.map constraintRect m, g)

constraintRect :: Verlet -> Verlet
constraintRect v
  | x (pos v) - r v < -width = v {pos = (-width + r v, y $ pos v)}
  | x (pos v) + r v > width = v {pos = (width - r v, y $ pos v)}
  | y (pos v) - r v < -height = v {pos = (x $ pos v, -height + r v)}
  | y (pos v) + r v > height = v {pos = (x $ pos v, height - r v)}
  | otherwise = v

constraintCircle :: Verlet -> Verlet
constraintCircle v
  | d > rad - r v = v {pos = p ^+^ ((to_obj /^ d) *^ (rad - r v))}
  | otherwise = v
  where
    p = (0, 0)
    rad = 400
    to_obj = pos v ^-^ p
    d = dist to_obj

solveCollisions :: World -> [(Int, Int)] -> World
solveCollisions = foldl checkCollision

-- have gone about this wrong, need to update pos then check collide
verletCollision :: World -> (Int, Int) -> World
verletCollision w'@(w, g) (i, j) | i /= j = fromMaybe w' $ do
  v <- Map.lookup i w
  u <- Map.lookup j w
  let axis = pos v ^-^ pos u
      d = dist axis
  if d < r v + r u
    then
      let n = axis /^ d
          delta = r v + r u - d
          posU = pos u ^-^ (n *^ (0.5 * delta))
          posV = pos v ^+^ (n *^ (0.5 * delta))
          grid_id_u_old = normalizePos (pos u)
          grid_id_v_old = normalizePos (pos v)
          grid_id_u_new = normalizePos posU
          grid_id_v_new = normalizePos posV
          g' =
            updateGrid
              g
              [ grid_id_v_new,
                grid_id_u_new,
                grid_id_v_old,
                grid_id_u_old
              ]
              [ (i, True),
                (j, True),
                (i, False),
                (j, False)
              ]
       in return (Map.insert j (u {pos = pos u ^-^ (n *^ (0.5 * delta))}) $ Map.insert i (v {pos = pos v ^+^ (n *^ (0.5 * delta))}) w, g')
    else return w'
verletCollision w _ = w

updateGrid :: Grid -> [(Int, Int)] -> [(Int, Bool)] -> Grid
updateGrid g (x : xs) ((i, True) : ys) = case Map.lookup x g of
  Just xs' -> updateGrid (Map.insert x (xs' ++ [i]) g) xs ys
  Nothing -> updateGrid (Map.insert x [i] g) xs ys
updateGrid g (x : xs) ((i, False) : ys) = case Map.lookup x g of
  Just xs' -> updateGrid (Map.insert x (filter (== i) xs') g) xs ys
  Nothing -> updateGrid (Map.insert x [0] g) xs ys
updateGrid g [] [] = g
updateGrid g _ [] = g
updateGrid g [] _ = g

checkCollision :: World -> (Int, Int) -> World
checkCollision w@(_, g) k =
  let verlets = fromMaybe [] $ Map.lookup k g
      v1 = pairs'' verlets (fromMaybe [] $ Map.lookup (fst k, snd k + 1) g)
      v2 = pairs'' verlets (fromMaybe [] $ Map.lookup (fst k, snd k - 1) g)
      v3 = pairs'' verlets (fromMaybe [] $ Map.lookup (fst k + 1, snd k) g)
      v4 = pairs'' verlets (fromMaybe [] $ Map.lookup (fst k - 1, snd k) g)
   in foldl verletCollision w (v1 ++ v2 ++ v3 ++ v4)

dist :: Point -> Float
dist p = sqrt (x p ** 2 + y p ** 2)

pairs :: World -> [(Int, Int)]
pairs (w, _) = [(x, y) | x <- xs, y <- xs, x /= y]
  where
    xs = map fst (Map.toList w)

pairs' :: [Int] -> [(Int, Int)]
pairs' xs = [(x, y) | x <- xs, y <- xs]

pairs'' :: [Int] -> [Int] -> [(Int, Int)]
pairs'' xs ys = [(x, y) | x <- xs, y <- ys]

normalizePos :: (Float, Float) -> (Int, Int)
normalizePos (x, y)
  | x < 0 && y > 0 = roundTuple $ normalizeNormalized (width - abs x, height - abs y)
  | x < 0 && y < 0 = roundTuple $ normalizeNormalized (width - abs x, abs y + height)
  | x > 0 && y < 0 = roundTuple $ normalizeNormalized (abs x + width, abs y + height)
  | x > 0 && y > 0 = roundTuple $ normalizeNormalized (abs x + width, height - abs y)
  | otherwise = (round x, round y)

normalizeNormalized :: (Float, Float) -> (Float, Float)
normalizeNormalized (x, y)
  | x < 0 = (0, y)
  | x > width * 2 = (width * 2, y)
  | y < 0 = (x, 0)
  | y > height * 2 = (x, height * 2)
  | x < 0 && y < 0 = (0, 0)
  | x < 0 && y > height * 2 = (0, height * 2)
  | x > width * 2 && y < 0 = (width * 2, 0)
  | x > width * 2 && y > height * 2 = (width * 2, height * 2)
  | otherwise = (x, y)

roundTuple :: (Float, Float) -> (Int, Int)
roundTuple (x, y) = (round x, round y)

{-# OPTIONS_GHC -Wno-type-defaults #-}

module Verlet where

import Graphics.Gloss

type Acceleration = Point

data Verlet = Verlet
  { pos :: Point,
    prev_pos :: Point,
    a :: Acceleration,
    r :: Float
  }
  deriving (Eq, Read, Show)

updatePosition :: Float -> Verlet -> Verlet
updatePosition dt v = v {pos = p, prev_pos = prev, a = a1}
  where
    velocity = pos v ^-^ prev_pos v
    prev = pos v
    p = pos v ^+^ velocity ^+^ (a v *^ (dt * dt))
    a1 = (0, 0)

accelerate :: Acceleration -> Verlet -> Verlet
accelerate acc v = v {a = a v ^+^ acc}

x :: Point -> Float
x = fst

y :: Point -> Float
y = snd

(^-^) :: Point -> Point -> Point
(^-^) p1 p2 = (x p1 - x p2, y p1 - y p2)

(^+^) :: Point -> Point -> Point
(^+^) p1 p2 = (x p1 + x p2, y p1 + y p2)

(^*^) :: Point -> Point -> Point
(^*^) p1 p2 = (x p1 * x p2, y p1 * y p2)

(^/^) :: Point -> Point -> Point
(^/^) p1 p2 = (x p1 / x p2, y p1 / y p2)

(*^) :: Point -> Float -> Point
(*^) p1 f = (x p1 * f, y p1 * f)

(/^) :: Point -> Float -> Point
(/^) p1 f = (x p1 / f, y p1 / f)

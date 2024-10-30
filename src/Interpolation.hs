module Interpolation (linearInterpolation, lagranInterpolation, Message) where

type Message = (Point, String)

type Point = (Double, Double)

linearInterpolation :: [Point] -> Double -> Double -> [Message]
linearInterpolation p step nowX = case p of
  [] -> []
  [(_, _)] -> []
  ((x1, y1) : (x2, y2) : _) -> case compare nowX x1 of
    LT -> linearInterpolation p step (nowX + step)
    EQ -> ((nowX, y1), "\nLinear interpolation:\n") : linearInterpolation p step (nowX + step)
    GT -> case compare nowX x2 of
      LT -> ((nowX, linFunc nowX), "") : linearInterpolation p step (nowX + step)
      EQ -> ((nowX, y2), "end") : linearInterpolation (tail p) step ((fst . head . tail) p)
      GT -> ((nowX, linFunc nowX), "end") : linearInterpolation (tail p) step ((fst . head . tail) p)
    where
      linFunc x = k * x + b
      k = (y2 - y1) / (x2 - x1)
      b = y1 - k * x1

lagranInterpolation :: [Point] -> Double -> Double -> [Message]
lagranInterpolation p step nowX = case p of
  [] -> []
  [(_, _)] -> []
  [(_, _), (_, _)] -> []
  ((x1, y1) : (x2, y2) : (x3, y3) : _) -> case compare nowX x1 of
    LT -> lagranInterpolation p step (nowX + step)
    EQ -> ((nowX, y1), "\nLargange Interpolation:\n") : lagranInterpolation p step (nowX + step)
    GT -> case compare nowX x3 of
      LT -> ((nowX, polynom nowX), "") : lagranInterpolation p step (nowX + step)
      EQ -> ((nowX, y3), "end") : lagranInterpolation (tail p) step ((fst . head . tail) p)
      GT -> ((nowX, polynom nowX), "end") : lagranInterpolation (tail p) step ((fst . head . tail) p)
    where
      polynom x = y1 * l0 x + y2 * l1 x + y3 * l2 x
      l0 x = ((x - x2) * (x - x3)) / ((x1 - x2) * (x1 - x3))
      l1 x = ((x - x1) * (x - x3)) / ((x2 - x1) * (x2 - x3))
      l2 x = ((x - x1) * (x - x2)) / ((x3 - x1) * (x3 - x2))

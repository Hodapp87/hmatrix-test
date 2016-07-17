module Main where

import Debug.Trace (trace)
import Numeric.LinearAlgebra
import Numeric.LinearProgramming

count = 25
-- Bug is present only with count >= 25
p0 = (2><count) $ repeat 1

-- Bug is not present if I change [Matrix R] and [Double R], which I
-- use only as lists with two elements, to tuples
recurse :: Int -> Int -> [Matrix R] -> [Matrix R]
recurse _ 0 m = m
recurse x iters m = recurse x (iters - 1) d
  where a = f $ map (\q -> flatten $ q ? [x]) m
        d = zipWith (\v q -> accum q (\_ _ -> v) [((x, 1), v)]) a m

f :: [Vector R] -> [Double]
f [p, q] = let obj = zipWith (+) (toList p) (toList q)
               -- Addition above seems necessary for the bug
               constr = Dense $ [ replicate count 1 :==: 1 ]
               bounds = map (:&: (0,1)) [1..count]
               tryA = case simplex (Maximize obj) constr bounds of
                 Optimal  (s, l) -> let r = vector l in [p <.> r, q <.> r]
                 Feasible (s, l) -> let r = vector l in [p <.> r, q <.> r]
                 -- 'Feasible' case must be present above for bug
                 t@_ -> error ("Failed at try A: " ++ show t ++ ", obj=" ++ show obj)
               tryB = case simplex (Maximize obj) constr bounds of
                 Optimal  (s, l) -> let r = vector l in [p <.> r, q <.> r]
                 t@_ -> error ("Failed at try B: " ++ show t ++ ", obj=" ++ show obj)
  in seq tryA tryB

main :: IO ()
main = do
  putStrLn $ show $ recurse 1 100 [p0, p0]

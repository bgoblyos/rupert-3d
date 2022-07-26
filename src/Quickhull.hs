module Quickhull
( isListContained
, quickHull2d
) where

import General

quickHull2d xs = [a] ++ findHull set2 b a ++ [b] ++ findHull set1 a b
  where a = leftMost xs
        b = rightMost xs
        set1 = filter (isRight a b) xs
        set2 = filter (isRight b a) xs

findHull [] _ _ = []
findHull xs a b = findHull set2 c b ++ [c] ++ findHull set1 a c
  where c = maxDist a b xs
        outside = filter (not . isInsideTriangle a b c) xs
        set1 = filter (isRight a c) outside
        set2 = filter (isRight c b) outside



maxDist a b = foldl (\acc x -> if dist x > dist acc then x else acc) a
  where dist x = lineDistance a b x

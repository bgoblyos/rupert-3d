module Quickhull
( quickHull2d
) where

isRight [ax, ay] [bx, by] [cx, cy] = (x1*y2) < (x2*y1) -- check for negative 3rd coordinate
  where x1 = bx - ax -- vector between the two points defining the oriented line
        y1 = by - ay
        x2 = cx - ax -- vector between first point of line and tested point
        y2 = cy - ay

quickHull2d xs = [a] ++ findHull set2 b a ++ [b] ++ findHull set1 a b
  where a = leftMost xs
        b = rightMost xs
        set1 = filter (isRight a b) xs
        set2 = filter (isRight b a) xs

findHull [] _ _ = []
findHull xs a b = findHull set2 c b ++ [c] ++ findHull set1 a c
  where c = maxDist a b xs
        outside = filter (not . isInside a b c) xs
        set1 = filter (isRight a c) outside
        set2 = filter (isRight c b) outside

isInside a b c x = originalArea == subAreaSum
  where originalArea = triangleArea a b c
        subArea1 = triangleArea a b x
        subArea2 = triangleArea b c x
        subArea3 = triangleArea c a x
        subAreaSum = subArea1 + subArea2 + subArea3

triangleArea [ax, ay] [bx, by] [cx, cy] = 0.5 * abs ((ax-cx)*(by - ay) - (ax - bx)*(cy - ay))

leftMost xs = foldl (\[mx ,my] [x, y] -> if x < mx then [x, y] else [mx, my]) firstValue xs
  where firstValue = head xs

rightMost xs = foldl (\[mx ,my] [x, y] -> if x > mx then [x, y] else [mx, my]) firstValue xs
  where firstValue = head xs

maxDist a b = foldl (\acc x -> if dist x > dist acc then x else acc) a
  where dist x = lineDistance a b x

lineDistance [x1, y1] [x2, y2] [x0, y0] = area / baseLength
  where area = 2 * triangleArea [x1, y1] [x2, y2] [x0, y0]
        baseLength = sqrt ((x2-x1)^2 + (y2-y1)^2)

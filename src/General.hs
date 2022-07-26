module General
  ( isRight
  , isInsideTriangle
  , leftMost
  , rightMost
  , lineDistance
  , polygonArea
  , isListContained ) where

isRight [ax, ay] [bx, by] [cx, cy] = (x1*y2) < (x2*y1) -- check for negative 3rd coordinate
  where x1 = bx - ax -- vector between the two points defining the oriented line
        y1 = by - ay
        x2 = cx - ax -- vector between first point of line and tested point
        y2 = cy - ay

isInsideTriangle a b c x = originalArea == subAreaSum
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

lineDistance [x1, y1] [x2, y2] [x0, y0] = area / baseLength
  where area = 2 * triangleArea [x1, y1] [x2, y2] [x0, y0]
        baseLength = sqrt ((x2-x1)^2 + (y2-y1)^2)

isPointContained xs p = all (isRightFlipped p) sides
  where sides = zip xs (tail xs ++ [head xs])
        isRightFlipped p (a, b) = isRight a b p

isListContained xs = all (isPointContained xs)

midPoint = zipWith avg
  where avg x y = (x+y)/2

polygonArea xs = sum $ map (triangleArea' middlePoint) sides
  where middlePoint = midPoint (leftMost xs) (rightMost xs)
        sides = zip xs (tail xs ++ [head xs])
        triangleArea' p (a, b) = triangleArea a b p

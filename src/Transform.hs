module Vector
  ( rotateList
  , projectList
  ) where

projectList theta phi = map (project theta phi)

rotateList alpha = map (rotate alpha)

-- matrix representation: | a b |
--                        | c d |

--
matrixMult2D [[a, b], [c, d]] [x, y] = vecAdd (scale x [a, c]) (scale y [b, d])

vecAdd = zipWith (+)

scale x = map (*x)

rotate alpha = matrixMult2D rotMatrix
  where rotMatrix = [ [cos alpha, - sin alpha]
                    , [sin alpha, cos alpha] ]

project theta phi [x, y, z] = 0

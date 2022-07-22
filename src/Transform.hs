module Transform
  ( rotateList
  , projectList
  ) where

import Data.List

projectList theta phi = map (project theta phi)

rotateList alpha = map (rotate alpha)

-- matrix is represented row by row, like: [ [a, b]    ->  | a b |
--                                         , [c, d] ]  ->  | c d |

matrixMult matrix vector = vecSum $ zipWith scale vector columns
  where columns = transpose matrix

vecAdd = zipWith (+)

vecSum = foldl1 vecAdd

scale x = map (*x)

rotate alpha = matrixMult rotMatrix
  where rotMatrix = [ [cos alpha, - sin alpha]
                    , [sin alpha, cos alpha] ]

project theta phi = matrixMult projectMatrix
  where projectMatrix = [ [ - sin theta             , cos theta                , 0       ] 
                        , [ (- cos theta) * cos phi , (- sin theta ) * cos phi , sin phi ] ]

module Polyhedra
  ( cube
  , octahedron
  , triakisTetrahedron ) where 

import Data.List

cube ::  [[Double]]
cube = [ [x, y, z] | x <- [-1,1], y <- [-1,1], z <- [-1,1]]

octahedron = nub $ permutations [0,0,1] ++
                   permutations [0,0,-1]

triakisTetrahedron =
  let
    a = (9 * sqrt 2) / 20
    b = (3 * sqrt 2) / 4
  in
    [ [ c * p1, c * p2, c * p3] | c <- [a,b], 
                                  p1 <- [-1, 1],
                                  p2 <- [-1, 1],
                                  p3 <- [-1, 1],
                                  p1 * p2 * p3 == 1 ]

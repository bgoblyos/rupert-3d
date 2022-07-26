module Main where

import Plot
import Quickhull
import Transform
import General
import Sample
import Data.List
import Random.MWC.Pure (seed)
import Graphics.EasyPlot

main = do
  
  mapM_ print filtered
  massPlot filtered poly 100

poly = octahedron
range = 10^4
seed1 = 2892422396
seed2 = 2566929885
seed3 = 1139884487
seed4 = 47210163
seed5 = 626567986

massPlot results polyhedron limit = mapM_ (plotResult polyhedron) zipped
  where zipped = zip results [1..limit]

plotResult polyhedron ([theta1, phi1, theta2, phi2, alpha], index) =
  plot (SVG ("plots/" ++ show index ++ ".svg")) [ Data2D [Title "Projection 1"] [] (pList points1)
                        , Data2D [Title "Projection 2"] [] (pList points2)
                        , Data2D [Title "Convex hull 1", Style Lines] [] (pList wrappedHull1)
                        , Data2D [Title "Convex hull 2", Style Lines] [] (pList wrappedHull2) ]
                        where points1 = rotateList alpha $ projectList theta1 phi1 polyhedron
                              points2 = projectList theta2 phi2 polyhedron
                              hull1 = quickHull2d points1
                              hull2 = quickHull2d points2
                              wrappedHull1 = hull1 ++ [head hull1]
                              wrappedHull2 = hull2 ++ [head hull2]
                              toTuple2 [x,y] = (x, y)
                              pList = map toTuple2

filtered = filter (testRupert poly) randomParams

randomParams = take range $ transpose [r (0, 2*pi) seed1, map acos $ r (-1, 1) seed2, r (0, 2*pi) seed3, map acos $ r (-1, 1) seed4, r (0, 2*pi) seed5]
  where r (lo, hi) newSeed = listGenerator (lo, hi) (seed [newSeed])

cube = [[1,1,1],[1,1,-1],[1,-1,1],[1,-1,-1],[-1,1,1],[-1,1,-1],[-1,-1,1],[-1,-1,-1]]

cube' = [[x, y, z] | x <- [1, -1], y <- [1, -1], z <- [1, -1]]

octahedron = [[0,0,1],[0,0,-1],[0,1,0],[0,-1,0],[1,0,0],[-1,0,0]]

testRupert polyhedron [theta1, phi1, theta2, phi2, alpha] = isListContained bigger smaller
  where poly1 = quickHull2d $ rotateList alpha $ projectList theta1 phi1 polyhedron
        poly2 = quickHull2d $ projectList theta2 phi2 polyhedron
        (bigger, smaller) = areaSort poly1 poly2
        areaSort x y = if polygonArea x > polygonArea y
                          then (x, y)
                          else (y, x)

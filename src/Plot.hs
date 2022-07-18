module Plot
( plotHull
) where

import Graphics.EasyPlot
import Data.List

plotHull points hull = plot X11 [ Data2D [Title "Convex hull", Style Lines] [] wrappedHull
                                , Data2D [Title "Convex hull Points"] [] (printableList hull)
                                , Data2D [Title "Non-hull points"] [] nonhull ]
                                  where wrappedHull = printableList $ hull ++ [head hull]
                                        nonhull = printableList $ points \\ hull

printableList = map toTuple2

toTuple2 [x,y] = (x, y)

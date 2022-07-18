module Plot
( plotHull
) where

import Graphics.EasyPlot
import Data.List

plotHull points hull = plot X11 [ Data2D [Title "Convex hull", Style Lines] [] wrappedHull
                                , Data2D [Title "Convex hull Points"] [] hull
                                , Data2D [Title "Non-hull points"] [] nonhull ]
                                  where wrappedHull = hull ++ [head hull]
                                        nonhull = points \\ hull

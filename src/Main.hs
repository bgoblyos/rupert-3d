import Plot
import Quickhull

main = do
  plotHull points hull

points = [[0,0], [1,1], [5,6], [5,8], [-7,9], [-3,-7], [-5,3], [-2,4], [3,3], [-1,6], [5,5], [4,-4], [0.5, 0.7], [5.7, 3.5]]
hull = quickHull2d points


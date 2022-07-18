module Sample
  ( randomList
  ) where

import System.Random
import Random.MWC.Pure
import System.IO.Unsafe

randomList (lo, hi) = listGenerator (lo, hi) newSeed
  where newSeed = seed $ map unsafePerformIO $ replicate 16 getRandom
        -- this should be moved to Main to enable 
        -- saving the seed for reproducable results

listGenerator (lo, hi) oldSeed = num : listGenerator (lo, hi) newSeed
  where result = range_random (lo, hi) oldSeed
        num = fst result
        newSeed = snd result

getRandom = do randomIO

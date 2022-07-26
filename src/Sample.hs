module Sample
  ( listGenerator
  ) where

import Random.MWC.Pure

listGenerator (lo, hi) oldSeed = num : listGenerator (lo, hi) newSeed
  where result = range_random (lo, hi) oldSeed
        num = fst result
        newSeed = snd result

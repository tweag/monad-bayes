{-|
Module      : Control.Monad.Bayes.Herding
Description : Kernel herding algorithms
Copyright   : (c) Adam Scibior, 2017
License     : MIT
Maintainer  : ams240@cam.ac.uk
Stability   : experimental
Portability : GHC

-}

module Control.Monad.Bayes.Herding (
  module Control.Monad.Bayes.Kernel,
  herding
  ) where

import Data.Bifunctor
import Numeric.LinearAlgebra
import Control.Monad.State
import qualified Data.Vector as Vector

import Control.Monad.Bayes.Kernel

-- | Kernel herding greedily selects pseudo-samples from a distribution
-- in a way that minimizes MMD between the original ditribution and one
-- obtained from pseudo-samples.
-- The algorithm is a slight modification of the one described by
-- Chen et al. (2010).
-- The input distribution should be normalized, otherwise spurious results
-- may be produced.
herding :: Kernel R a -> [(a,R)] -> [a]
herding _ [] = error "Herding: empty input list"
herding k ps = evalState (sequence $ repeat selectNext) start where
  -- the implementation uses a vector of scores updated with each pseudo-sample
  -- at each step the best score is selected to choose the next pseudo-sample

  -- preprocessing
  (xs,ws) = second (vector) $ unzip ps
  vectorXs = Vector.fromList xs
  kmat = kernelMatrix k xs xs

  -- initial value of scores
  start = constUpdate - scale 0.5 (takeDiag kmat)

  -- updates to scores at each step depending on the index of the value chosen
  constUpdate = kmat #> ws
  varUpdate i = negate (kmat ! i)
  update i as = as + constUpdate + varUpdate i

  -- selecting next pseudo-sample and updating scores in a state monad
  selectNext = do
    as <- get
    let i = maxIndex as
    modify (update i)
    return (vectorXs Vector.! i)

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

herding :: Kernel R a -> [(a,R)] -> Int -> [a]
herding _ [] _ = error "Herding: empty input list"
herding _ _ n | n < 0 =
  error "Herding: negative number of pseudo-samples requested"
herding k ps n = evalState (sequence $ replicate n selectNext) start where
  -- the implementation uses a vector of scores updated with each pseudo-sample
  -- at each step the best score is selected to choose the next pseudo-sample

  -- preprocessing
  (xs,ws) = second vector $ unzip ps
  vectorXs = Vector.fromList xs
  kmat = kernelMatrix k xs xs

  -- initial value of scores
  start = ws - scale 0.5 (takeDiag kmat)

  -- updates to scores at each step depending on the index of the value chosen
  constUpdate = kmat #> ws
  varUpdate i = negate (kmat ! i)
  update i as = as + constUpdate - varUpdate i

  -- selecting next pseudo-sample and updating scores in a state monad
  selectNext = do
    as <- get
    let i = maxIndex as
    modify (update i)
    return (vectorXs Vector.! i)

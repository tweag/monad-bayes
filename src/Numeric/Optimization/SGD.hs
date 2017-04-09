{-|
Module      : Numeric.Optimization.SGD
Description : Stochastic gradient descent
Copyright   : (c) Adam Scibior, 2017
License     : MIT
Maintainer  : ams240@cam.ac.uk
Stability   : experimental
Portability : GHC

-}

module Numeric.Optimization.SGD (
  SGDParam,
  sga
) where

data SGDParam r = SGDParam {learningRate :: r, decayRate :: r, steps :: Int}

validateSGDParam :: SGDParam r -> SGDParam r
validateSGDParam p@(SGDParam _ _ s) = check `seq` p where
  check = if s < 0 then error "SGDParam: number of steps was negative" else ()

sga :: (Monad m, Traversable t, Num r) => SGDParam r -> (t r -> m (t (r,r))) -> t r -> m (t r)
sga param f xs0 = go (steps param) (learningRate param) xs0 where
  go 0 _ xs = return xs
  go n r xs = do
    xs' <- step r xs
    go (n-1) (r * decayRate param) xs'

  step rate xs = do
    ys <- f xs
    return $ fmap (\(x, gx) -> x + rate * gx) ys

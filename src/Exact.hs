
module Exact where

--Algorithms for exact inference

import Base
import Dist

--------------------------------------------------------------------
--Rejection sampling

-- | Uses rejection sampling to generate exact samples from the posterior.
-- The first argument is an upper bound on likelihood scores.
-- The tighter this bound, the more efficient the algorithm is.
rejectionSampling :: (Monad d, Bayesian d, Bernoulli d) =>
                   Prob -> d a -> d a
rejectionSampling cap d =
    iterate where
        p = prior d
        iterate = do
              (x,s) <- p
              keep  <- bernoulli (s / cap)
              if keep then return x else iterate

-- | A more efficient version of 'rejectionSampling' where rejection
-- step is preformed after each conditional.
stepRej :: Prob -> Dist a -> Dist a
stepRej cap (Conditional c d) =
    iterate where
        exactD = stepRej cap d
        iterate = do
          x    <- exactD
          keep <- bernoulli (c x / cap)
          if keep then return x else iterate
stepRej cap (Bind d f) = stepRej cap d >>= f
--Non-recursive cases are not conditional, so don't have to be modified
stepRej cap d = d


module BetaBin where

-- The beta-binomial model in latent variable and urn model representations.
-- The two formulations should be exactly equivalent, but only urn works with Dist.

import Control.Monad.State.Lazy (get,put,evalStateT)
import Data.Number.LogFloat

import Base

-- | Beta-binomial model as an i.i.d. sequence conditionally on weight.
latent :: MonadDist d => Int -> d [Bool]
latent n = do
  weight <- uniform 0 1
  let toss = bernoulli (logFloat weight)
  sequence $ replicate n $ toss

-- | Beta-binomial as a random process.
urn :: MonadDist d => Int -> d [Bool]
urn n = flip evalStateT (1,1) $ do
          let toss = do
                (a,b) <- get
                let weight = a / (a + b)
                outcome <- bernoulli (logFloat weight)
                let (a',b') = if outcome then (a+1,b) else (a,b+1)
                put (a',b')
                return outcome
          sequence $ replicate n $ toss

-- | Post-processing by counting the number of True values.
count :: [Bool] -> Int
count = length . filter id

-- | A beta-binomial model where the first three states are True,True,False.
-- The resulting distribution is on the remaining outcomes.
cond :: MonadBayes d => d [Bool] -> d [Bool]
cond d = do
  (first:second:third:rest) <- d
  condition (first  == True)
  condition (second == True)
  condition (third  == False)
  return rest

-- | The final conditional model, abstracting the representation.
model :: MonadBayes d => (Int -> d [Bool]) -> Int -> d Int
model repr n = fmap count $ cond $ repr (n+3) 




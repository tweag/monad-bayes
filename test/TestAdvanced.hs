module TestAdvanced where

import ConjugatePriors
  ( betaBernoulli',
    betaBernoulliAnalytic,
    gammaNormal',
    gammaNormalAnalytic,
    normalNormal',
    normalNormalAnalytic,
  )

-- passed1 = sampled $ unweighted $ mh 10 $

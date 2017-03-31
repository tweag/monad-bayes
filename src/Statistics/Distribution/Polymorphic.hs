{-|
Module      : Statistics.Distribution.Polymorphic
Description : Common probability distributions
Copyright   : (c) Adam Scibior, 2017
License     : MIT
Maintainer  : ams240@cam.ac.uk
Stability   : experimental
Portability : GHC

-}

{-# LANGUAGE
  MultiParamTypeClasses
 #-}

-- Probability distributions defined polymorphically over arbitrary numeric types.
-- This module reexports for each distribution its type, a safe constructor, and a sampling helper.
-- For additional functions import specific distribution modules.
module Statistics.Distribution.Polymorphic (
  module Statistics.Distribution.Polymorphic.Normal,
  module Statistics.Distribution.Polymorphic.Gamma,
  module Statistics.Distribution.Polymorphic.Beta,
  module Statistics.Distribution.Polymorphic.Uniform,
  module Statistics.Distribution.Polymorphic.Discrete,
  module Statistics.Distribution.Polymorphic.MVNormal
) where

import Statistics.Distribution.Polymorphic.Normal (Normal, normalDist, normal)
import Statistics.Distribution.Polymorphic.Gamma (Gamma, gammaDist, gamma)
import Statistics.Distribution.Polymorphic.Beta (Beta, betaDist, beta)
import Statistics.Distribution.Polymorphic.Uniform (Uniform, uniformDist, uniform)
import Statistics.Distribution.Polymorphic.Discrete (Discrete, discreteDist, discrete)
import Statistics.Distribution.Polymorphic.MVNormal (MVNormal, mvnormalDist, mvnormal)

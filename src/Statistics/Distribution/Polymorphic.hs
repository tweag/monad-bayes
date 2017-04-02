{-|
Module      : Statistics.Distribution.Polymorphic
Description : Common probability distributions polymorphic in numeric types
Copyright   : (c) Adam Scibior, 2017
License     : MIT
Maintainer  : ams240@cam.ac.uk
Stability   : experimental
Portability : GHC

-}

-- Probability distributions defined polymorphically over arbitrary numeric types.
-- This module reexports for each distribution its type and a safe constructor.
-- For additional functions import specific distribution modules.
module Statistics.Distribution.Polymorphic (
  module Numeric.LogDomain,
  module Statistics.Distribution.Polymorphic.Class,
  module Statistics.Distribution.Polymorphic.Normal,
  module Statistics.Distribution.Polymorphic.Gamma,
  module Statistics.Distribution.Polymorphic.Beta,
  module Statistics.Distribution.Polymorphic.Uniform,
  module Statistics.Distribution.Polymorphic.Discrete,
  module Statistics.Distribution.Polymorphic.MVNormal
) where

import Numeric.LogDomain (LogDomain, NumSpec)
import Statistics.Distribution.Polymorphic.Class
import Statistics.Distribution.Polymorphic.Normal (Normal, normalDist)
import Statistics.Distribution.Polymorphic.Gamma (Gamma, gammaDist)
import Statistics.Distribution.Polymorphic.Beta (Beta, betaDist)
import Statistics.Distribution.Polymorphic.Uniform (Uniform, uniformDist)
import Statistics.Distribution.Polymorphic.Discrete (Discrete, discreteDist)
import Statistics.Distribution.Polymorphic.MVNormal (MVNormal, mvnormalDist)

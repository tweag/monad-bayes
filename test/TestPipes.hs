module TestPipes where

import Control.Monad.Bayes.Class ()
import Control.Monad.Bayes.Enumerator ( enumerate )
import Data.AEq ( AEq((~==)) )
import BetaBin (urn, urnP)


urns :: Int -> Bool
urns n = enumerate (urn n) ~== enumerate (urnP n)

{-# OPTIONS_GHC -Wno-monomorphism-restriction #-}

module TestPipes where

import BetaBin (urn, urnP)
import Control.Monad.Bayes.Class ()
import Control.Monad.Bayes.Enumerator (enumerate)
import Data.AEq (AEq ((~==)))
import HMM (hmm, hmmPosterior)
import Pipes ((>->))
import Pipes.Prelude (toListM)
import qualified Pipes.Prelude as Pipes

urns :: Int -> Bool
urns n = enumerate (urn n) ~== enumerate (urnP n)

hmms :: [Double] -> Bool
hmms observations =
  let hmmWithoutPipe = hmm observations
      hmmWithPipe = reverse . init <$> toListM (hmmPosterior observations)
   in enumerate hmmWithPipe ~== enumerate hmmWithoutPipe

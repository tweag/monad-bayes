{-# OPTIONS_GHC -Wno-monomorphism-restriction #-}

module TestPipes where

import BetaBin (urn, urnP)
import Control.Monad.Bayes.Class ()
import Control.Monad.Bayes.Enumerator (enumerator)
import Data.AEq (AEq ((~==)))
import Data.List (sort)
import HMM (hmm, hmmPosterior)
import Pipes.Prelude (toListM)

urns :: Int -> Bool
urns n = enumerator (urn n) ~== enumerator (urnP n)

hmms :: [Double] -> Bool
hmms observations =
  let hmmWithoutPipe = hmm observations
      hmmWithPipe = reverse . init <$> toListM (hmmPosterior observations)
   in -- Sort enumerator again although it is already sorted, see https://github.com/tweag/monad-bayes/issues/283
      sort (enumerator hmmWithPipe) ~== sort (enumerator hmmWithoutPipe)

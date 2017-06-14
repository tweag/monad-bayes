module TestProposal where

import Test.Hspec
import Data.AEq

import Control.Monad.Bayes.Simple
import Control.Monad.Bayes.Enumerator
import Control.Monad.Bayes.Inference.Proposal

spec :: Spec
spec = do
  describe "Proposal distributions" $ do
    it "preserve posterior with discrete distributions" $ prop_posterior

noProposal :: Dist Double Int
noProposal = do
  x <- discrete [0.5,0.3,0.2]
  y <- discrete [0.3,0.4,0.3]
  return (x+y)

withProposal :: Dist Double Int
withProposal = do
  x <- (discreteDist [0.5,0.3,0.2]) `proposingFrom` (discreteDist [0.2,0.2,0.2,0.4])
  y <- (discreteDist [0.3,0.4,0.3]) `proposingFrom` (discreteDist $ if x > 1 then [0.4,0.4,0.2] else [0.5,0.25,0.25])
  return (x+y)

prop_posterior :: Bool
prop_posterior = noProposal ~== withProposal

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}

module Main (main) where

import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Sampler
import Control.Monad.Reader
import System.Random.MWC (createSystemRandom)

main :: IO ()
main = do
  g <- createSystemRandom
  xs <- sampleWith (replicateM 1000000 random) g
  print $ sum xs

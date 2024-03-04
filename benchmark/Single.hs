{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}

import Control.Applicative (Applicative (..))
import Control.Monad.Bayes.Sampler.Strict
import Data.Time (diffUTCTime, getCurrentTime)
import Helper
import Options.Applicative
  ( ParserInfo,
    auto,
    execParser,
    fullDesc,
    help,
    info,
    long,
    maybeReader,
    option,
    short,
  )
-- Prelude exports liftA2 from GHC 9.6 on, see https://github.com/haskell/core-libraries-committee/blob/main/guides/export-lifta2-prelude.md
-- import Control.Applicative further up can be removed once we don't support GHC <= 9.4 anymore

import Prelude hiding (Applicative (..))

infer :: Model -> Alg -> IO ()
infer model alg = do
  x <- sampleIOfixed (runAlg model alg)
  print x

opts :: ParserInfo (Model, Alg)
opts = flip info fullDesc $ liftA2 (,) model alg
  where
    model =
      option
        (maybeReader parseModel)
        ( long "model"
            <> short 'm'
            <> help "Model"
        )
    alg =
      option
        auto
        ( long "alg"
            <> short 'a'
            <> help "Inference algorithm"
        )

main :: IO ()
main = do
  (model, alg) <- execParser opts
  startTime <- getCurrentTime
  infer model alg
  endTime <- getCurrentTime
  print (diffUTCTime endTime startTime)

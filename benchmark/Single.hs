{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}

import Control.Monad.Bayes.Sampler.Strict
import Data.Time (diffUTCTime, getCurrentTime)
import Helper
import Options.Applicative
  ( Applicative (liftA2),
    ParserInfo,
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

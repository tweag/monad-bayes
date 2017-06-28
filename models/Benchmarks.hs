import Criterion.Main
import Numeric.AD
import System.Random.MWC (createSystemRandom, GenIO)

import Numeric.LogDomain
import Statistics.Distribution.Polymorphic
import Control.Monad.Bayes.Simple
import Control.Monad.Bayes.Sampler

pdfBenchmarkable :: (Density d) => d -> Domain d -> Benchmarkable
pdfBenchmarkable d = whnf (toLog . pdf d)

pdfBenchmark :: (Density d) => d -> Domain d -> Benchmark
pdfBenchmark d x = bench "density" $ pdfBenchmarkable d x

benchN :: String -> [Int] -> (Int -> Benchmarkable) -> Benchmark
benchN name ns bmark = bgroup name $ map (\n -> bench (show n) (bmark n)) ns

defNs :: [Int]
defNs = [1,10]

defDiscrete :: Int -> Discrete Double
defDiscrete n = discreteDist $ replicate n 1

pdfBenchmarks :: Benchmark
pdfBenchmarks = bgroup "pdf" [
  bgroup "normal" [ pdfBenchmark (normalDist 1 5) (4 :: Double),
                    bench "grad" $ whnf (head . grad (toLog . pdf (normalDist 1 5) . head )) [4 :: Double]
                  ],
  bgroup "gamma"  [ pdfBenchmark (gammaDist 2 3) (5 :: Double),
                    bench "grad" $ whnf (head . grad (toLog . pdf (gammaDist 2 3) . head )) [5 :: Double]
                  ],
  bgroup "beta"   [ pdfBenchmark (betaDist 2 4) (0.5 :: Double),
                    bench "grad" $ whnf (head . grad (toLog . pdf (normalDist 2 4) . head )) [0.5 :: Double]
                  ],
  bgroup "uniform" [ pdfBenchmark (uniformDist 7 10) (8 :: Double),
                    bench "grad" $ whnf (head . grad (toLog . pdf (normalDist 7 10) . head )) [8 :: Double]
                  ],
  benchN "discrete" defNs (\n -> pdfBenchmarkable (defDiscrete n) 0)
  ]

samplingBenchmarks :: GenIO -> Benchmark
samplingBenchmarks g = bgroup "sample" [
  bench "normal"  $ whnfIO $ sampleIOwith (normal 1 5)   g,
  bench "gamma"   $ whnfIO $ sampleIOwith (gamma 2 3)    g,
  bench "beta"    $ whnfIO $ sampleIOwith (beta 2 4)     g,
  bench "uniform" $ whnfIO $ sampleIOwith (uniform 7 10) g,
  benchN "discrete" defNs (\n -> whnfIO $ sampleIOwith (sample $ defDiscrete n) g)
  ]


main :: IO ()
main = do

  g <- createSystemRandom

  let benchmarks = [
        pdfBenchmarks,
        samplingBenchmarks g
        ]

  defaultMain benchmarks

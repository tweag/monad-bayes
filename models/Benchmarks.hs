import Criterion.Main
import Numeric.AD

import Numeric.LogDomain
import Statistics.Distribution.Polymorphic

pdfBenchmark :: (Density d) => d -> Domain d -> Benchmark
pdfBenchmark d x = bench "pdf" $ whnf (toLog . pdf d) x

main :: IO ()
main = defaultMain [
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
                  ]
  ]

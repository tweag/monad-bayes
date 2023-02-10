module TestStormerVerlet
  ( passed1,
  )
where

import Control.Lens
import Control.Monad.ST
import Data.Maybe (fromJust)
import qualified Data.Vector as V
import qualified Linear as L
import Linear.V
import Math.Integrators.StormerVerlet
import Statistics.Function (square)

gConst :: Double
gConst = 6.67384e-11

nStepsTwoPlanets :: Int
nStepsTwoPlanets = 44

stepTwoPlanets :: Double
stepTwoPlanets = 24 * 60 * 60 * 100

sunMass, jupiterMass :: Double
sunMass = 1.9889e30
jupiterMass = 1.8986e27

jupiterPerihelion :: Double
jupiterPerihelion = 7.405736e11

jupiterV :: [Double]
jupiterV = [-1.0965244901087316e02, -1.3710001990210707e04, 0.0]

jupiterQ :: [Double]
jupiterQ = [negate jupiterPerihelion, 0.0, 0.0]

sunV :: [Double]
sunV = [0.0, 0.0, 0.0]

sunQ :: [Double]
sunQ = [0.0, 0.0, 0.0]

tm :: V.Vector Double
tm = V.enumFromStepN 0 stepTwoPlanets nStepsTwoPlanets

keplerP :: L.V2 (L.V3 Double) -> L.V2 (L.V3 Double)
keplerP (L.V2 p1 p2) = L.V2 dHdP1 dHdP2
  where
    dHdP1 = p1 / pure jupiterMass
    dHdP2 = p2 / pure sunMass

keplerQ :: L.V2 (L.V3 Double) -> L.V2 (L.V3 Double)
keplerQ (L.V2 q1 q2) = L.V2 dHdQ1 dHdQ2
  where
    r = q2 L.^-^ q1
    ri = r `L.dot` r
    rr = ri * (sqrt ri)
    q1' = pure gConst * r / pure rr
    q2' = negate q1'
    dHdQ1 = q1' * pure sunMass * pure jupiterMass
    dHdQ2 = q2' * pure sunMass * pure jupiterMass

listToV3 :: [a] -> L.V3 a
listToV3 [x, y, z] = fromV . fromJust . fromVector . V.fromList $ [x, y, z]
listToV3 xs = error $ "Only supply 3 elements not: " ++ show (length xs)

initPQ2s :: L.V2 (L.V2 (L.V3 Double))
initPQ2s =
  L.V2
    (L.V2 (listToV3 jupiterQ) (listToV3 sunQ))
    (L.V2 (pure jupiterMass * listToV3 jupiterV) (pure sunMass * listToV3 sunV))

result2 :: V.Vector (L.V2 (L.V2 (L.V3 Double)))
result2 = runST $ integrateV (\h -> stormerVerlet2H (pure h) keplerQ keplerP) initPQ2s tm

energy :: (L.V2 (L.V2 (L.V3 Double))) -> Double
energy x = keJ + keS + peJ + peS
  where
    qs = x ^. _1
    ps = x ^. _2
    qJ = qs ^. _1
    qS = qs ^. _2
    pJ = ps ^. _1
    pS = ps ^. _2
    keJ = (* 0.5) $ (/ jupiterMass) $ sum $ fmap square pJ
    keS = (* 0.5) $ (/ sunMass) $ sum $ fmap square pS
    r = qJ L.^-^ qS
    ri = r `L.dot` r
    peJ = 0.5 * gConst * sunMass * jupiterMass / (sqrt ri)
    peS = 0.5 * gConst * sunMass * jupiterMass / (sqrt ri)

energies :: V.Vector Double
energies = fmap energy result2

diffs :: V.Vector Double
diffs = V.zipWith (\x y -> abs (x - y) / x) energies (V.tail energies)

passed1 :: IO Bool
passed1 = return $ V.all (< 1.0e-3) diffs

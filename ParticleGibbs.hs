{-# LANGUAGE TupleSections #-}

module ParticleGibbs (pgibbs) where

import Control.Arrow (first,second)
import Control.Monad (join)

import Base
import Explicit (Explicit(Explicit))
import Dist
import Importance
import SMC hiding (toExplicit)

-- | A container where each particle also carries a distribution
-- over the next set of particles conditional on retaining the current value.
data GibbsParticles a = GibbsParticles [((a,Dist (GibbsParticles a)), Prob)]
toSamples (GibbsParticles ps) = ps

condTrace :: Int -> Dist a -> Dist (a, Dist (GibbsParticles a))
condTrace n (Conditional c d) = condition (c . fst) $ do
  (x, g) <- condTrace n d
  let retain (x,g) = (x, next) where
        next = do
          GibbsParticles ps <- g
          --reweight the particles
          let qs = map (\((x,n),w) -> ((x,n), w * c x)) ps
          --resample n-1 new particles
          xs <- sequence $ replicate (n-1) $ categorical qs
          --retain the selected particle and retain recursively
          let ys = (x, next) : map retain xs
          --ys <- sequence $ replicate n $ categorical qs
          --reset weights to uniform
          let w = 1 / fromIntegral n
          return $ GibbsParticles $ map (, w) ys
  return $ retain (x,g)
condTrace n (Bind d f) = condTrace n d >>= advance where
  advance (x,g) = do
    y <- f x
    --subsequent runs
    let next = do
        GibbsParticles ps <- g
        --strip weights
        let (xs,ws) = unzip ps
        --recursively update subsequent runs
        ys <- mapM advance xs
        --retain selected particle
        let zs = zip ((y,next) : tail ys) ws
        return $ GibbsParticles $ zip ys ws
    return (y,next)
condTrace n d = do
    x <- d
    let w = 1 / fromIntegral n
    let next = fmap (GibbsParticles . (((x,next),w):) . replicate (n-1) . (, w)) (condTrace n d)
    return (x,next)


-- | The Particle Gibbs algorithm.
-- Uses the specified number of particles to construct an infinite Markov chain.
pgibbs :: Int -> Dist a -> Dist [Samples a]
pgibbs n d = (smcStandard n $ condTrace n d) >>= unwield where
    unwield :: Samples (a, Dist (GibbsParticles a)) -> Dist [Samples a]
    unwield d = do
        (x,g) <- categorical d
        next  <- g
        rest  <- unwield $ toSamples next
        return (map (first fst) d : rest)

pgibbs' :: Int -> Int -> Dist a -> Dist a
pgibbs' k n d = fmap (!! k) $ (smcStandard n $ condTrace n d) >>= unwield where
    unwield :: Samples (a, Dist (GibbsParticles a)) -> Dist [a]
    unwield d = do
        (x,g) <- categorical d
        next  <- g
        rest  <- unwield $ toSamples next
        return (x : rest)


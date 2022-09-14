{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments, FlexibleContexts, ImportQualifiedPost, LambdaCase, OverloadedStrings, TupleSections #-}


-- |
-- Module      : Control.Monad.Bayes.Sampler
-- Description : Pseudo-random sampling monads
-- Copyright   : (c) Adam Scibior, 2015-2020
-- License     : MIT
-- Maintainer  : leonhard.markert@tweag.io
-- Stability   : experimental
-- Portability : GHC
--
-- 'SamplerIO' and 'SamplerST' are instances of 'MonadSample'. Apply a 'MonadCond'
-- transformer to obtain a 'MonadInfer' that can execute probabilistic models.
module Control.Monad.Bayes.Sampler.Strict
  -- ( Sampler,
  --   SamplerIO,
  --   SamplerST,
  --   sampleIO,
  --   sampleIOfixed,
  --   sampleWith,
  --   sampleSTfixed,
  --   sampleMean,
  --   sampler,
  -- )
where

import Control.Foldl qualified as F hiding (random)
import Control.Monad.Bayes.Class
import Control.Monad.Reader 
import Control.Monad.ST (ST)
import Numeric.Log (Log (ln))
import System.Random.MWC.Distributions qualified as MWC
import System.Random.Stateful (IOGenM (..), STGenM, StatefulGen, StdGen, initStdGen, mkStdGen, newIOGenM, newSTGenM, uniformDouble01M, uniformRM)
import Data.MonadicStreamFunction.Core
import Control.Monad.Bayes.Sequential.Coroutine
import Control.Monad.Bayes.Population
import Control.Monad.Identity
import Control.Arrow
import Control.Monad.IO.Class

import FRP.Rhine

-- rhine-gloss
import FRP.Rhine.Gloss.IO
import Data.MonadicStreamFunction.InternalCore (MSF(..))

import Data.Functor
-- rhine-bayes
-- import FRP.Rhine.Bayes hiding (average)
import Numeric.Log hiding (sum)
import Data.Tuple (swap)
import FRP.Rhine.Gloss.Common
import GHC.Float (float2Double, double2Float)
import qualified Control.Monad.Trans.MSF.Reader as DunaiReader

type StdDev = Double
type Pos = Double
type Sensor = Pos

-- | The sampling interpretation of a probabilistic program
-- Here m is typically IO or ST
newtype Sampler g m a = Sampler (ReaderT g m a) deriving (Functor, Applicative, Monad, MonadIO)

-- | convenient type synonym to show specializations of Sampler
-- to particular pairs of monad and RNG
type SamplerIO = Sampler (IOGenM StdGen) IO

-- | convenient type synonym to show specializations of Sampler
-- to particular pairs of monad and RNG
type SamplerST s = Sampler (STGenM StdGen s) (ST s)

instance StatefulGen g m => MonadSample (Sampler g m) where
  random = Sampler (ReaderT uniformDouble01M)

  uniform a b = Sampler (ReaderT $ uniformRM (a, b))
  normal m s = Sampler (ReaderT (MWC.normal m s))
  gamma shape scale = Sampler (ReaderT $ MWC.gamma shape scale)
  beta a b = Sampler (ReaderT $ MWC.beta a b)

  bernoulli p = Sampler (ReaderT $ MWC.bernoulli p)
  categorical ps = Sampler (ReaderT $ MWC.categorical ps)
  geometric p = Sampler (ReaderT $ MWC.geometric0 p)

-- | Sample with a random number generator of your choice e.g. the one
-- from `System.Random`.
--
-- >>> import Control.Monad.Bayes.Class
-- >>> import System.Random.Stateful hiding (random)
-- >>> newIOGenM (mkStdGen 1729) >>= sampleWith random
-- 4.690861245089605e-2
sampleWith :: StatefulGen g m => Sampler g m a -> g -> m a
sampleWith (Sampler m) = runReaderT m

-- | initialize random seed using system entropy, and sample
sampleIO, sampler :: SamplerIO a -> IO a
sampleIO x = initStdGen >>= newIOGenM >>= sampleWith x
sampler = sampleIO

-- | Run the sampler with a fixed random seed
sampleIOfixed :: SamplerIO a -> IO a
sampleIOfixed x = newIOGenM (mkStdGen 1729) >>= sampleWith x

-- | Run the sampler with a fixed random seed
sampleSTfixed :: SamplerST s b -> ST s b
sampleSTfixed x = newSTGenM (mkStdGen 1729) >>= sampleWith x

sampleMean :: [(Double, Log Double)] -> Double
sampleMean samples =
  let z = F.premap (ln . exp . snd) F.sum
      w = (F.premap (\(x, y) -> x * ln (exp y)) F.sum)
      s = (/) <$> w <*> z
   in F.fold s samples




  --  type StdDev = Double
type Pos2 = (Double, Double)
type Sensor2 = Pos2


model4 :: forall cl m td. (Float ~ Time cl, MonadSample m) => ClSF m cl () Pos2
-- forall m td . BehaviourF m td Double Double
model4 = feedback (0,0) model4' >>> arr (\(x,y) -> (double2Float x, double2Float y)) 
  >>> decayIntegral 1 >>> decayIntegral 1
  >>> arr (\(x,y) -> (float2Double x, float2Double y)) where 
  model4' = MSF \((), (x,y)) -> do
    time <- DunaiReader.asks sinceLast -- @(TimeInfo cl) @ReaderT
    x' <- normal x (float2Double (time*2))
    y' <- normal y (float2Double (time*2))
    let p = (x',y')
    return ((p,p), model4')


model3 :: (MonadSample m, Diff td ~ Double) => BehaviourF m td StdDev (Sensor2, Pos2)
model3 = feedback zeroVector $ proc (stdDev, position') -> do
  impulse <- arrM (normal 0) &&& arrM (normal 0) -< stdDev
  -- FIXME make -3 input, sample once at the beginning, or on every key stroke
  let acceleration = (-3) *^ position' ^+^ impulse
  -- Integral over roughly the last 100 seconds, dying off exponentially, as to model a small friction term
  velocity <- arr (^+^ (0, 10)) <<< decayIntegral 100 -< acceleration
  position <- integralFrom (10, 0) -< velocity
  measurementError <- constM (normal 0 2) &&& constM (normal 0 2) -< ()
  returnA -< ((position ^+^ measurementError, position), position)



model2 :: (MonadIO m, MonadSample m, Diff td ~ Float) => BehaviourF m td StdDev (Sensor, Pos)
model2 = proc stdDev -> do
  position <- arrM (const $ read <$> liftIO getLine) -< ()
  measurementError <- constM $ normal 0 3 -< ()
  returnA -< (float2Double position + measurementError, float2Double position)
   


model :: (MonadSample m, Diff td ~ Float) => BehaviourF m td StdDev (Sensor, Pos)
model = proc stdDev -> do
  acceleration <- arrM $ normal 5 -< stdDev
  -- Integral over roughly the last 100 seconds, dying off exponentially
  velocity <- decayIntegral 2 -< double2Float acceleration
  -- Integral over velocity with very slow reset
  position <- decayIntegral 2 -< velocity
  measurementError <- constM $ normal 0 3 -< ()
  returnA -< (float2Double position + measurementError, float2Double position)

decayIntegral :: (VectorSpace v (Diff td), Monad m) => Diff td -> BehaviourF m td v v
decayIntegral timeConstant = average timeConstant >>> arr (timeConstant *^)

sensor :: (MonadSample m, Diff td ~ Float) => BehaviourF m td StdDev Sensor
sensor = model >>> arr fst

filtered :: (MonadInfer m, Diff td ~ Float) => BehaviourF m td (StdDev, Sensor) Pos
filtered = proc (stdDev, sensor) -> do
  (estimatedOutput, latent) <- model -< stdDev
  arrM factor -< normalPdf estimatedOutput 1 sensor -- FIXME I think this is called an importance function?
  returnA -< latent
-- filtered = bayesFilter model

-- FIXME Can't do it with Has?
-- mainClSF :: (MonadIO m, MonadInfer m, Has (ExceptT ()) m) => BehaviourF m td () ()

type MySmallMonad = IdentityT (GlossConcT SamplerIO)

data Result = Result
  { estimate :: Pos
  , stdDev :: StdDev
  , measured :: Sensor
  , latent :: Pos
  }
  deriving Show

filteredAndTrue :: Diff td ~ Float => BehaviourF MySmallMonad td StdDev Result
filteredAndTrue = proc stdDev -> do
  (measuredPosition, actualPosition) <- model -< stdDev
  samples <- runPopulationCl 200 resampleMultinomial filtered -< (stdDev, measuredPosition)
  -- arrM $ liftIO . print -< samples
  returnA -< Result
    { estimate = averageOf samples
    , stdDev = stdDevOf samples
    , measured = measuredPosition
    , latent = actualPosition
    }

-- Use Statistical?
averageOf :: VectorSpace v n => [(v, Log n)] -> v
averageOf things =
  let
    properThings = first (exp . ln) . swap <$> things
    fullWeight = sum $ fst <$> properThings
    sumOfThings = foldr (^+^) zeroVector $ fmap (uncurry (*^)) properThings
  in sumOfThings ^/ fullWeight

stdDevOf :: [(Double, Log Double)] -> Double
stdDevOf things =
  let
    average = averageOf things
    squares = first (\x -> (x - average) ^ 2) <$> things
  in sqrt $ averageOf squares

visualisation :: BehaviourF MySmallMonad td Result ()
visualisation = proc Result { estimate, stdDev, measured, latent } -> do
  constMCl $ lift clearIO -< ()
  drawBall -< (estimate, stdDev, blue)
  drawBall -< (measured, 0.3, red)
  drawBall -< (latent, 0.3, withAlpha 0.5 green)

visualisation2 :: BehaviourF MySmallMonad td Pos2 ()
visualisation2 = proc (x,y) -> do
  constMCl $ lift clearIO -< ()
  drawBall2 -< ( x,y, blue)

-- FIXME opacity of estimate based on total probability mass

drawBall2 :: BehaviourF MySmallMonad td (Double, Double, Color) ()
drawBall2 = proc (x, y, theColor) -> do
  arrMCl $ lift . paintIO -< scale 20 20 $ translate (double2Float x) (double2Float y) $ color theColor $ circleSolid $ 1

drawBall :: BehaviourF MySmallMonad td (Double, Double, Color) ()
drawBall = proc (position, width, theColor) -> do
  arrMCl $ lift . paintIO -< scale 20 20 $ translate (double2Float position) 0 $ color theColor $ circleSolid $ double2Float width

mainClSF :: (Diff td ~ Float, td ~ Float) => BehaviourF MySmallMonad td () ()
-- mainClSF :: BehaviourF MyMonad td () ()
mainClSF = proc () -> do

  -- output <- model4 -< ()
  -- visualisation2 -< output


  let stdDev = 20
  output <- filteredAndTrue -< stdDev
  visualisation -< output
  arrM $ liftIO . print -< output
  n <- count -< ()
  arrM $ liftIO . print -< n

  -- liftHS $ throwOn () -< n > 100
  -- liftClSF $ liftClSF $ throwOn () -< n > 100

-- liftHS :: Has t m => (forall n . ClSF (t n) cl a b) -> ClSF m cl a b
-- liftHS clsf = hoistClSF liftH clsf

type MyMonad = Sequential (Population SamplerIO)
-- type MyMonad = Sequential (Population (ExceptT () SamplerIO))

cl :: IOClock MySmallMonad (Millisecond 100)
-- cl :: IOClock MyMonad (Millisecond 100)
cl = ioClock waitClock
-- cl :: LiftClock (Population SamplerIO) Sequential (LiftClock SamplerIO Population (Millisecond 1000))
-- cl = liftClock $ liftClock (waitClock @1000)

-- data MyClock = MyClock

-- instance MonadIO m => Clock m MyClock where
--   type Time MyClock = td
--   type Tag  MyClock = Bool
--   initClock MyClock = do
--     (initClock cl


-- See, this is why we need effect frameworks.
-- Or why monad-bayes needs newtypes for monad transformers
instance MonadSample m => MonadSample (GlossConcT m) where
  random = lift random
  -- FIXME Other PDs?

glossClock :: LiftClock (GlossConcT SamplerIO) IdentityT GlossSimClockIO
glossClock = liftClock GlossSimClockIO

main2 = do
  -- TODO would like push
  thing <- sampleIO
    -- $ runExceptT @()
    -- $ evidence
    -- $ smcMultinomial 10000 10
    $ launchGlossThread defaultSettings
      { display = InWindow "rhine-bayes" (1024, 960) (10, 10) }
    $ runIdentityT
    $ reactimateCl glossClock mainClSF
  print thing

runPopulationCl :: forall m cl a b . Monad m =>
-- | Number of particles
  Int ->
-- | Resampler
  (forall x . Population m x -> Population m x)
  -> ClSF (Population m) cl a b
  -> ClSF m cl a [(b, Log Double)]
runPopulationCl nParticles resampler = DunaiReader.readerS . runPopulationS nParticles resampler . DunaiReader.runReaderS


runPopulationS :: forall m a b . Monad m =>
  -- | Number of particles
  Int ->
  -- | Resampler
  (forall x . Population m x -> Population m x)
  -> MSF (Population m) a b
  -> MSF m a [(b, Log Double)]
runPopulationS nParticles resampler msf = runPopulationCl' $ spawn nParticles $> msf
  where
    runPopulationCl' :: Monad m => Population m (MSF (Population m) a b) -> MSF m a [(b, Log Double)]
    runPopulationCl' msfs = MSF $ \a -> do
      -- TODO This is quite different than the dunai version now. Maybe it's right nevertheless.
      bAndMSFs <- runPopulation $ flip unMSF a =<< msfs
      -- FIXME This abominal lambda could be done away by using Weighted?
      let (currentPopulation, continuations) = unzip $ (\((b, msf), weight) -> ((b, weight), (msf, weight))) <$> bAndMSFs
      -- FIXME This normalizes, which introduces bias, whatever that means
      return (currentPopulation, runPopulationCl' $ resampler $ fromWeightedList $ return continuations)

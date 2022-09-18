# Functional reactive probabilistic programming

A domain that monad-bayes doesn't handle easily is online inference. This is a setting where there is an incoming stream of data, and inference needs to be performed in real time, incrementally. (This is the setting in which one might use a Kalman filter, for example).

GIF

One might want those inputs to come from multiple streams, at different rates, or even from user input.

One might even want to take actions in real time, based on the current belief state, which would in turn influence incoming data.

Monad-bayes is great at handling **distributions** in a pure and functional manner, but doesn't have a good model of **time**. However, there are packages which can handle **time** in a pure and functional way, but don't know about **distributions**.

As such, it turns out that there is a beautiful synergy between approaches to modeling time (specifically real-time interactive processes) known as *Functional Reactive Programming* (FRP) and monad-bayes. The interaction between this two domains is beautifully seamless, as usual with Haskell.

The high level view is that we can represent *stochastic* processes (both discrete and continuous) as FRP-style signals, paramterized by a probability monad `m`. We can write a variant of sequential Monte Carlo, using monad-bayes' composable inference algorithms, to infer these processes. 


Rather than give a thorough tutorial of the packages or ideas involved (the ideas being probabilistic and functional reactive programming, and the packages being `dunai`, `Rhine` and `monad-bayes`), I'll give a very thoroughly discussed run-through of how the above gif was made:


You can find the code in full, with imports and so on at 
...


As shown in the gif above, our goal is to simulate a tracking system for an agent (the green circle), which moves around the screen. Over time, we receive noisy measurements of its position, and use these, in conjunction with a prior, to determine its position. 

We'll represent 
  todo

```haskell
std :: Double
std = 5

type Observation = V.Vector 2 Double
type Position = V.Vector 2 Double
```

To give a sense of how a *non*-stochastic process like a circle moving on a screen can be represented in the FRP paradigm, here's an example

TODO

The type comes from `Rhine`, an FRP library. This library is suitable for two reasons: 1) it has a very type-aware model of time, and 2) it is built on `dunai`, a generalization of stream processing to arbitrary monads, which forms the ideal basis for a probabilistic generalization of FRP.

  blah
  denotes a signal *function* from `a` to `b`. Think of this 
     (i.e. a function from one signal to another)

Arrow notation, as seen above, is very useful here, because it allows us to manipulate signals directly. We'll use it going forward.

The next step is to build a *stochastic* process that represents our *belief* about an object's trajectory. 


```haskell
prior = V.fromTuple <$> (model1D &&& model1D) where

    model1D = proc _ -> do
        acceleration <- constM (normal 0 5) -< ()
        velocity <- decayIntegral 2 -< acceleration -- Integral, dying off exponentially
        position <- decayIntegral 2 -< velocity
        returnA -< position

    decayIntegral timeConstant = average timeConstant >>> arr (timeConstant *^)

```
    
I call this `prior` because it is going to represent our belief about the ball's position prior to any measurement. 

It is built by taking a constantly normally distributed stream of accelerations, and integrating it twice. That gives the circle's movement on one axis, so we join two of these together.

Note how this is an abstract mathematical object: it is not an event loop for example - it is simply a representation of a stochastic process. But of course we can convert it into something we can run.


```haskell
main = sampleIO $ reactimateCl clock (prior >>> arrM (liftIO . print))
```

`reactimateCl` takes a signal 
  blah m () ()

`(prior >>> arrM (liftIO . print))` has just that type. It represents a signal which returns the unit value constantly, but runs a side effect of printing the current output of `prior` at every tick.

So `reactimateCl clock (prior >>> arrM (liftIO . print))` is a distribution (concretely represented as a sampler) from which we can sample. Drawing a single sample incurs the whole act of marching indefinitely forward through time, printing the value of `prior` at each point.

Very nice.

And things continue to be nice, because we can now easily define the (stochastic signal) that represents our observations over time, by passing it through a signal transformation:


```haskell
generativeModel = proc p -> do
    n <- fmap V.fromTuple $ noise &&& noise -< ()
    returnA -< p + n

    where noise = constM (normal 0 std)
```

This generates noise at every point from a normal distribution, and gives us the resulting stream.

We can view that as follows:

example


We now want to represent the *posterior* process, namely the stochastic process describing your belief (varying over time, of course) about the position of the ball *given the observations*. This again turns out to be straightforward:

```haskell
posterior :: UnnormalizedDistribution m => StochasticSignalTransform m Observation Position
posterior = proc (V2 oX oY) -> do
  latent@(V2 trueX trueY) <- prior -< ()
  arrM factor -< normalPdf oY std trueY * normalPdf oX std trueX
  returnA -< latent
```

Again, this is just an abstract description of the posterior process. Note that the posterior is itself a signal *function*: it is going to take an input signal (the observations) and return a (distribution over) output signals, or rather, a stochastic process.


To be able to convert it into something runnable, we first need to perform inference.

In a manner typical of monad-bayes, we do so by taking `posterior` and performing a measure-preserving transformation to turn it into a more convenient representation. This new representation is as a stream which at any given time is a population of particles, each with a weight, such that the whole population is an approximation of the desired distribution.

```haskell
onlineSMC :: ...
```

tutorial:
  a stream
  a stream transformation:
    type S = () -> (a, S)
    type M = a -> Time -> (b, M)
    type M = a -> Dist (Time -> (b, M))

What is exciting about this?
- more complex generative models:
  - fix the issue with borel's paradox
  - try observing an integrated noise
  - try observing acceleration and guessing position
- inferring a static quantity:
  - guess mean of the acceleration
  - is pmmh possible here? if model is prior + stochastic process
  - Hamiltonian
- adaptive filters:
  - resample, mh step or spawn when the variance is too high
- exception handling:
  - models where the ball teleports at random times
- inference in the loop
- multi-rate incoming streams and outgoing streams
  - sensor fusion: accelerometer + vision, etc
  - user input

----------
-- display
----------

    
visualisation :: StochasticSignalTransform (IdentityT (GlossConcT SamplerIO)) Result ()
visualisation = proc Result { estimate, stdDev, measured, latent, showEstimate, showObservation } -> do
  constMCl $ lift clearIO -< ()
  if showEstimate 
      then drawBall -< (estimate, stdDev, blue)
      else constM (pure ()) -< ()

  if showObservation
      then drawBall -< (measured, 0.3, red)
      else constM (pure ()) -< ()

  drawBall -< (latent, 0.3, withAlpha 0.5 green)

  where
    drawBall = proc (V2 x y, width, theColor) -> do
        arrMCl $ lift . paintIO -<
            scale 50 50 $
            translate (double2Float x) (double2Float y) $
            color theColor $
            circleSolid $
            double2Float width

gloss :: IO ()
gloss = sampleIO $
        launchGlossThread defaultSettings
            { display = InWindow "rhine-bayes" (1024, 960) (10, 10) } 
        do 
        -- (e, _) <- initClock $ GlossEventClockIO
        runIdentityT
         $ reactimateCl glossClock proc () -> do

                actualPosition <- blab -< ()
                measuredPosition <- generativeModel -< actualPosition
                samples <- onlineSMC 50 resampleMultinomial posterior -< measuredPosition
                -- (_, event) <- (readerS (constM (pure ()) >>> liftTransS e)) -< ()
                -- (readerS (constM (pure ()) >>> e) >>> arrM (lift . paintIO . text . show))
                visualisation -< Result { estimate = averageOf samples
                                    , stdDev = stdDevOf (first xCoord <$> samples) + stdDevOf (first yCoord <$> samples)
                                    , measured = measuredPosition
                                    , latent = actualPosition
                                    , showEstimate = True -- isChar 'b' event
                                    , showObservation = True -- isChar 'c' event
                                    }


isChar c' (EventKey (Char c) Down _ _ ) | c'==c = True
isChar _ _ = False

data Result = Result
  { estimate :: Position
  , stdDev :: Double
  , measured :: Observation
  , latent :: Position
  , showEstimate :: Bool
  , showObservation :: Bool
  }
  deriving Show

glossClock :: LiftClock (GlossConcT SamplerIO) IdentityT GlossSimClockIO
glossClock = liftClock GlossSimClockIO











```




Functional reactive programming (FRP) is a paradigm for describing real-time interactive systems, such as GUIs or games.


This document describes how it can be used to beautifully describe both discrete and continuous stochastic processes, and SMC inference on them.

## Functional Reactive Programming

dunai approach

continuous signals using dunai...

This is the approach taken by libraries like `Rhine`,


example of a non-stochastic model with Rhine

```haskell
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
```

## Modeling stochastic processes

A (continuous) stochastic process can be represented as

```haskell
type StochasticProcess a = forall m. MonadInfer m => SFCl m () a
```

and an operator from one stochastic process to another as:


todo

Example

```haskell
model :: (MonadSample m, Diff td ~ Float) => BehaviourF m td StdDev (Sensor, Pos)
model = proc stdDev -> do
  acceleration <- arrM (normal 5) -< stdDev
  -- Integral over roughly the last 100 seconds, dying off exponentially
  velocity <- decayIntegral 2 -< double2Float acceleration
  -- Integral over velocity with very slow reset
  position <- decayIntegral 2 -< velocity
  measurementError <- constM $ normal 0 3 -< ()
  returnA -< (float2Double position + measurementError, float2Double position)
```

`arrM (normal 5) :: todo` is a

stochastic process which is entirely uncorrelated: at each time $t$ it produces a Gaussian with mean $ 5 $ and variance `stdDev`. Integration is an operator on signals that behaves as you would expect, so that `position` can be obtained by integrating acceleration twice. But note that we integrated a stochastic process, so the position output is also stochastic. While the acceleration had no time dependence, the position certainly does.

`constM (normal 0 3)` is another very simple 

operator, which given any input, produces a stochastic process that is `normal 0 3` at all times.

## Inference with stochastic processes

As with probabilistic programming in general, we maintain a principled separation between the model and inference. So far, we have discussed models of stochastic processes, but not inference.


```haskell
onlineSMC :: forall m cl a b . Monad m =>
    -- | Number of particles
  Int ->
        -- | Resampler
  (forall x . Population m x -> Population m x)
  -> ClSF (Population m) cl a b
  -> ClSF m cl a [(b, Log Double)]
```

This is going to transform the input process to produce a new one which runs a particle filter (i.e. Sequential Monte Carlo) that at every clock tick, will update with the new information since the last tick and then resample the population.

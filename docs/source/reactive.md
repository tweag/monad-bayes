# Functional reactive probabilistic programming

A domain that monad-bayes doesn't handle easily is online inference. This is a setting where there is an incoming stream of data, and inference needs to be performed in real time, incrementally. (This is the setting in which one might use a Kalman filter, for example).


One might even want to take actions in real time, based on the current belief state, which would in turn influence incoming data.


GIF


both model and inference

What sort of object describes a time-varying distribution?

How can one perform inference over such an object?




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

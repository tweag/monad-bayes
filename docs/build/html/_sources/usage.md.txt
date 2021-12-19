# Developer Guide

This document assumes the reader is familiar with:
- the basics of Bayesian probability theory
- basic Haskell (the syntax, the type system, do-notation, monad transformers)
- how to specify distributions in monad-bayes (see docs/probprog.md)

That's enough to understand the core ideas, but for the more advanced content, you'll also want to feel comfortable enough with Haskell's type system that free monads, free monad transformers, and coroutines aren't a barrier to entry. And of course, to understand how inference methods like MCMC and SMC are implemented, it doesn't hurt to understand how they work in a statistical sense.

## References

monad-bayes is the codebase accompanying the theory of probabilistic programming described in [this paper](https://www.denotational.co.uk/publications/scibior-kammar-ghahramani-funcitonal-programming-for-modular-bayesian-inference.pdf).

## The core typeclasses

The library relies on two core typeclasses `MonadSample` and `MonadCond`. `MonadInfer` is simply the union of the two, that is:

```haskell=
(MonadSample m, MonadCond m) => MonadInfer m
```

You can find these in `Control.Monad.Bayes.Class`. Here is `MonadSample`:

```haskell=
class Monad m => MonadSample m where
  random :: m Double
```

This one method, `random`, represents a uniform distribution over $[0,1]$. (`MonadSample` actually has a few other distributions, but that's not essential.)

What comes next is clever: you can define any other distribution you like in terms of `random`. As an example:

```haskell=
bernoulli :: MonadSample m => m Bool
bernoulli p = fmap (< p) random
```

That one is pretty simple. As a more complex example, here's how a normal distribution is defined:

```haskell=
normal m s = fmap (quantile (normalDistr m s)) random
```

`normalDistr` comes from a separate library `Statistics.Distribution.Normal` and `quantile (normalDistr m s) :: Double -> Double` is the inverse CDF of the normal, a deterministic function.

Again, to emphasize: **all of our randomness can be reduced to draws from a uniform distribution over the interval $[0,1]$**. 

So we now have a way of constructing distributions in a monadic fashion. As a simple example:

```haskell=
example1 :: MonadSample m => m Double
example1 = do
    x <- random
    y <- uniform 0 x
    return (x + y > 1.5)
```

Think of this as the procedure of first sampling uniformly from $[0,1]$, then from $[0,x]$, and then returning the Boolean $x + y > 1.5$. More precisely, this is the **marginal** probability of $x + y > 1.5$. 

Inference is TODO
 `example1`, of type `MonadSample m => m Double`, and turn it into something we can actually see. Like samples. Or a parameter of a Bernoulli distribution. Those are problems for the next section, which is concerned with *interpreting* `MonadSample` as something more concrete, namely an inference algorithm.

The core philosophy of monad-bayes is that we specify distributions (probabilistic programs, that is) in this abstract monadic typeclass, and then cash it out in a variety of concrete ways which allow for convenient inference algorithms.

Here's `MonadCond`. 

```haskell=
class Monad m => MonadCond m where
  score :: Log Double -> m ()
```

First, `Log Double` is defined in `Numeric.Log`, and is a wrapper for `Double`s which does multiplication in log-space. It comes with `Exp :: a -> Log a` and `ln :: Log a -> a`. This is because we don't want numerical problems when multiplying tiny probabilities, and we want clean typesafe separation of doubles and log-doubles.

`score` is less intuitive than `random`. It will become clearer when we discuss more concrete interpretations of `MonadCond`, but what's important to know here is that it is used to allow us to do the Bayesian part of probability, as exemplified by the following probabilistic program: 

```haskell=
bayesianExample = do
    x <- prior
    score (likelihood x)
    return x
```

TODO: explain `score` as a soft factor statement: downweights the probability of that program trace
The most intuitive way to understand `score` is to think of a probabilistic program as making a series of random choices which trace out a possible execution of the program. At any point in this series, we can interject a `score x` statement, where the value of `x` depends on the previous choices. This statement multiplies the weight of this "trace" by the score.


## Inference transformers

Now core idea of monad-bayes is that various monads will be made to be instances of `MonadSample`, `MonadCond` or both (i.e. an instance of `MonadInfer`), and different inference algorithms will operate using instances. This separates the specification of the model (which happens abstractly in `MonadInfer`) from the inference algorithm, which takes place in on of the concrete instances. The clever part of monad-bayes is that it allows this instances to be constructed in a modular way, using monad transformers. In the paper, these are termed *inference transformers* to emphasize that it doesn't really matter whether they satisfy the monad laws.

For example, to run weighted rejection sampling on a probabilistic program `p`, we can write `(sampleIO . runWeighted) p`. Here, `(sampleIO . runWeighted) :: Weighted SamplerIO a -> IO a`. So `p` gets understood as being of type `Weighted SamplerIO`, a type we'll encounter soon.

Some of these transformers are easy to understand (like `StateT Double`, while others (like the Church transformed Free monad transformer) lie on the more advanced side of things. Accordingly, the following tour of these types goes from easy to hard.

### Enumerator

This interpreter is in `Control.Monad.Bayes.Enumerator`. Here is the core datatype:


```haskell=
newtype Enumerator a = 
    Enumerator (WriterT (Product (Log Double)) [] a)
```

This merits a little unpacking. First, `Product` as a wrapper from TODO which makes the semigroup operator for numbers be multiplication, so that:

```haskell=
Product 2 <> Product 3 == Product 6`
```

Unpacking the definition of `Enumerator a`, it is isomorphic to:

```haskell=
[(a, (Product (Log Double)))]
```

So, a value of type `Enumerator Bool`, for instance, is a list of pairs of booleans along with a double, like:

```haskell=
[(False,0.8914),(True,0.1086)]
```

Also in `Control.Monad.Bayes.Enumerator` is a function `enumerate`, which has type:

```haskell=
enumerate :: Ord a => Enumerator a -> [(a, Double)]
```

We can write `enumerate sprinkler`. Why is this well typed? The idea is that `sprinkler` has type `forall m. MonadInfer m => m Bool`, and we *instantiate* that `m` as `Enumerator`.

But for this to be well-typed, we need `Enumerator` to be an instance of `MonadInfer`. For that, we need `Enumerator` to be a `MonadSample`, and a `MonadCond`. For that, we need it to be a `Monad`, and in turn, a `Functor`. In understanding these instance definition, we'll understand what what `Enumerator` is doing for us.


`Enumerator` is a monad automatically, because `WriterT a m` is a monad for `a` a monoid and `m` a monad. As needed, `[]` is a monad and `Log Double` is a monoid. But what does that monad actually **do**?

For instance, if I have a weighted list `l` like `[(False,0.8914),(True,0.1086)]` and a function `f :: Bool -> Enumerator a`, what is `l >>= f`? It takes each element in `l`, and passes it through `f`, to obtain a weighted list of weighted lists. Then it flattens it, by including each element in the inner lists in the resulting list, with the weight of the list multiplied by the weight of the element. 

This is the essence of propagating probability forward.

<!-- This is of course a representation of a discrete distribution, and as we might expect, `Enumerator` is only going to be useful when we are dealing with discrete distributions. -->

What remains is to define the `MonadSample` and `MonadCond` instances:


```haskell=
instance MonadSample Enumerator where
  random = error "Infinitely supported random variables not supported in Enumerator"
  bernoulli p = fromList [(True, (Exp . log) p), (False, (Exp . log) (1 - p))]

instance MonadCond Enumerator where
  score w = fromList [((), w)]

```

The first thing to notice is that `random` is actually not defined for `Enumerator`, and consequently, you can't handle any continuous distributions. This makes sense, because you can't represent continuous distributions (whose support is uncountably infinite) as a list of samples.

So really `Enumerator` isn't very general purpose. It's best understood as a didactic tool, rather than a real world inference algorithm. 

But it can handle discrete distributions. It does this via `bernoulli` (as well as `categorical`, which I've omitted). `bernoulli p` constructs the weighted list corresponding to a `bernoulli` distribution with parameter `p`.

Finally, `MonadCond`. 

```haskell=
example2 = do
    x <- bernoulli 0.5
    y <- bernoulli $ if x then 0.6 else 0.5
    return (y && x)
```

### SamplerIO

```haskell=
newtype SamplerIO a = SamplerIO (ReaderT GenIO IO a)
```

There are various different implementations of samplers that you could write, and several are in monad-bayes. This is the simplest. The idea is that we have a function `sampleIO :: SamperIO a -> IO a`, then:

```haskell=
print =<< sampleIO random
```

will give us a sample from `random`. Similarly for other distributions. This works via:

```haskell=
instance MonadSample SamplerST where
  random = fromMWC System.Random.MWC.uniform

  bernoulli p = fromMWC $ MWC.bernoulli p
```

But note that `SamplerIO` only has a `MonadSample` instance. This means that if you do `sampleIO sprinkler`, you will get: 

```haskell=
No instance for (MonadInfer SamplerIO)
```

This is to be expected. `SamplerIO` has no instance for `MonadCond`. To obtain sampler based interpretation of probabilistic programs with factor statements, we need to continue.

### Weighted m


```haskell=
newtype Weighted m a = Weighted (StateT (Log Double) m a)
```

A **key** difference to `Enumerator` and `SamplerIO` is the **kind** of `Weighted`. It takes not just a type `a :: Type` as argument, but also a type `m :: Type -> Type`.

`Weighted` is isomorphic to:

```haskell=
Log Double -> m (a, Log Double)
```

This is not, at least to me, very intuitive. So let's look at the instances:

```haskell=
instance Monad m => MonadCond (Weighted m) where
  score w = Weighted (modify (* w))
```

So if we write:

```haskell=
ex :: MonadSample m => Weighted m Bool
ex = do
    x <- bernoulli 0.5
    score (if b then 1 else 0)
    return x
```

then the result is that first, we draw a sample from a Bernoulli distribution from the **underlying** distribution `m`, and then multiply the state (which is a `Log Double`) by a number which depends on that sample. For convenience, we write `condition b = score (if b then 1 else 0)`. 

To unpack from `Weighted m a`, we use:

```haskell=
runWeighted :: Weighted m a -> m (a, Log Double)
runWeighted (Weighted m) = runStateT m 1
```

`Weighted m` is not an instance of `MonadSample`, but only as instance of `MonadCond` (and that, only when `m` is an instance of `Monad`). However, since `StateT` is a monad transformer, there is a function `lift :: m Double -> Weighted m Double`.

So if we take a `MonadSample` instance like `SamplerIO`, then `Weighted SamplerIO` is an instance of both `MonadSample` and `MonadCond`. Which means it is an instance of `MonadInfer`. 

So we can successfully write `(sampleIO . runWeighted) sprinkler` and get a program of type `IO (Bool, Log Double)`. When run, this will draw a sample from `sprinkler` along with an **unnormalized** density for that sample.


It's worth stopping here to remark on what's going on. What has happened is that the `m` in `forall m. MonadInfer m => m Bool` has been *instantiated* as `Weighted SamplerIO`. This is an example of how the interpreters for inference can be composed in modular ways.

Finally, there's a function 

```haskell=
hoist :: (forall x. m x -> n x) -> Weighted m a -> Weighted n a
```

This takes a natural transformation `m ~> n` and lifts it into a natural transformation `Weighted m ~> Weighted n`. Most of the inference transformers have a `hoist` function.


<!-- `Weighted` and `SamplerIO` are on the simpler side, but illustrate the key principles of monad-bayes, namely:

**composable stack of inference transformers, implemented as instances of `MonadSample` and `MonadCond` typeclasses.**

In a similar vein, `Population` and `Sequential` go together to handle Sequential Monte Carlo (SMC), and `Traced` is associated with Markov Chain Monte Carlo (MCMC). -->


### Population


```haskell=
newtype Population m a = Population (Weighted (ListT m) a)
```

So:

```haskell=
Population m a ~ [Log Double -> (a, Log Double)]
```

Note that while `ListT` isn't in general a valid monad transformer, we're not requiring it to be one here.

`Population` is used to represent a collection of particles (in the statistical sense), along with their weights. 

There are several useful functions associated with it:

```haskell=
spawn :: Monad m => Int -> Population m ()
spawn n = fromWeightedList $ pure $ replicate n ((), 1 / fromIntegral n)

pushEvidence ::
  MonadCond m =>
  Population m a ->
  Population m a

resampleGeneric ::
  MonadSample m =>
  -- | resampler
  (V.Vector Double -> m [Int]) ->
  Population m a ->
  Population m a

```

`spawn` spawns new particles. As an example:

```haskell=
enumerate (spawn 2 >> bernoulli 0.5)
```

gives

TODO

`resampleGeneric` takes a function to select a set of indices from a vector, and makes a new population by selecting those indices. 

`pushEvidence`  "normalizes the weights in the population, while at the same time incorporating the sum of the weights as a score in m."

### Sequential 

```haskell=
newtype Sequential m a = 
    Sequential {runSequential :: Coroutine (Await ()) m a}
```

This is a wrapper for the `Coroutine` type applied to the `Await` function from `Control.Monad.Coroutine`, which are defined thus:

```haskell=
newtype Coroutine s m r = Coroutine {
   resume :: m (Either (s (Coroutine s m r)) r)
   }

newtype Await x y = Await (x -> y)
```

Unpacking that:

```haskell=  
Sequential m a ~ m (Either (() -> Sequential m a) a)
```

As usual, `m` is going to be some other probability monad, so understand `Sequential m a` as representing a program which, after making a random choice or doing conditioning, we either obtain an `a` value, or a paused computation, which when resumed gets us back to a new `Sequential m a`.

(For more on coroutines, see the final article in: https://themonadreader.files.wordpress.com/2011/10/issue19.pdf.)

The monad instance for coroutines is as follows:

```haskell=
instance (Functor s, Monad m) => Monad (Coroutine s m) where
   return = pure
   t >>= f = Coroutine (resume t >>= apply f)
      where apply fc (Right x) = resume (fc x)
            apply fc (Left s) = return (Left (fmap (>>= fc) s))
```

The `MonadSample` instance follows from `lift : m a -> Coroutine (Await ()) m a`, defined as:

```haskell=
instance Functor s => MonadTrans (Coroutine s) where
   lift = Coroutine . liftM Right
```


The `MonadCond` instance has less trivial content:

```haskell=
-- | Execution is 'suspend'ed after each 'score'.
instance MonadCond m => MonadCond (Sequential m) where
  score w = lift (score w) >> suspend
```

First you take a `score` in the underlying `MonadCond` instance, and then you `suspend`, which means:

```haskell=
-- | A point where the computation is paused.
suspend :: Monad m => Sequential m ()
suspend = Sequential (Coroutine (return (Left (Await return))))
```
TODO: double check

We can move to the next suspension point with:

```haskell=
advance :: Monad m => Sequential m a -> Sequential m a
advance = Sequential . bounce extract . runSequential

```

and move through all with:

```haskell=
-- | Remove the remaining suspension points.
finish :: Monad m => Sequential m a -> m a
finish = pogoStick extract . runSequential
```

But most importantly, we can apply a natural transformer over the underlying monad `m` to only the current suspension.

```haskell=
hoistFirst :: (forall x. m x -> m x) -> Sequential m a -> Sequential m a
hoistFirst f = Sequential . Coroutine . f . resume . runSequential
```

As an example, consider:

TODO: Enumerator example with `trace`

When `m` is `Population n` for some other `n`, then `resampleGeneric` gives us one example of the natural transformation we want. In other words, operating in `Sequential (Population n)` works, and not only works but does something statistically interesting: particle filtering (aka SMC).


### FreeSampler

(*Difficulty: high*)

As with `Population m`, `FreeSampler m` is not often going to be used on its own, but instead as part of the Markov Chain Monte Carlo (MCMC) inference method.

`FreeSampler m` is best understood if you're familiar with the standard use of a free monad to construct a domain specific language. For probability in particular, see this [blog post](https://jtobin.io/simple-probabilistic-programming). Here's the definition:

```haskell=
newtype SamF a = Random (Double -> a)
newtype FreeSampler m a = 
    FreeSampler {runFreeSampler :: FT SamF m a}
    
instance Monad m => MonadSample (FreeSampler m) where
  random = FreeSampler $ liftF (Random id)
```

The monad-bayes implementation uses a more efficient implementation of `FreeT`, from the `free` package, known as the *Church transformed Free monad*. But that only changes the operational semantics - performance aside, it works just the same as the standard `FreeT` datatype. 

If you unpack the definition, you get:

```haskell=

FreeSampler m a ~ m (Either a (Double -> (FreeSampler m a)))
```

As you can see, this is rather like `Coroutine`.

Since `FreeT` is a transformer, we can use `lift` to get a `MonadSample` instance.


A *trace* of a program of type `MonadSample m => m a` is an execution of the program, so a choice for each of the random values. Recall that`random` underlies all of the random values in a program, a trace for a program is fully specified by a list of `Double`s, giving the value of each call to `random`.

Given a probabilistic program interpreted in `FreeSampler m`, we can "run" it to produce a program in the underlying monad `m`. For simplicity, consider the case of a program `x` of type `FreeSampler Identity Bool`. We can then use the following function:

```haskell=
withRandomness :: Monad m => [Double] -> FreeSampler m a -> m a
```

This takes a list of `Double`s (a trace), and gives us:

TODO double check: do withPartialRandomness
```haskell=
runIdentity $ withRandomness listOfDoubles x :: a
```

The intuition here is that given a list of doubles in $[0,1]$, you can evaluate any probabilistic program. If your list of numbers is shorter than the number of calls to `random` in the program, the remaining calls are made in the underlying `MonadSample` instance `m`. 


<!-- This uses `iterTM`, one of the rather expressive folding patterns in `Control.Monad.Trans.Free` (from the *free* package) and a clever use of the state monad transformer. The upshot is that you supply a  -->


### Traced

(*Difficulty: hard*)


`Traced m` is actually several related interpretations, each built on top of `FreeSampler`. These range in complexity and power.



<!-- Control flow is stochastic in a probabilistic program, which is to say that whether we draw from some variable may depend on a draw from some variable earlier. This means that we don't have a fixed structure like a Bayes net corresponding to a probabilistic program. -->

The reason traces are relevant here is that monad-bayes implements a version of Markov Chain Monte Carlo (MCMC) that operates on arbitrary probabilistic programs TODO LINK. The idea is that the MCMC chain takes place on traces of the program. A step consitutes a change to this trace, i.e. to the list of `Double`s. For instance, the algorithm for an MH step goes as follows:

- propose a new trace by randomly redrawing a single `Double` in the trace
- accept or reject with the MH criterion

It's convenient to specify a trace not just as a `[Double]` but also with the resulting output, and the density of that output. This is what monad-bayes does:

```haskell=
data Trace a = Trace
  { 
    variables :: [Double],
    output :: a,
    density :: Log Double
  }
```

We also need a specification of the probabilistic program in question, free of any particular interpretation. That is precisely what `FreeSampler` is for. In particular, we 

run a trace on a freesampler program


The simplest is in `Control.Monad.Bayes.Traced.Basic`

```haskell=
Traced m a ~ (FreeSampler Identity a, Log Double), m (Trace a))
```

A `Traced` interpretation of a model is a particular run of the model with its corresponding probability, alongside a distribution over `Trace` info, which records: the value of each call to `random`, the value of the final output, and the density of this program trace.

This machinery allows us to implement MCMC. In particular, this version of MCMC performs its random walk on program traces of type `Trace a`. A single step in this chain (in Metropolis Hasting MCMC) looks like this:

```haskell=
mhTrans :: MonadSample m => 
    Weighted (FreeSampler m) a -> Trace a -> m (Trace a)
mhTrans m t@Trace {variables = us, density = p} = do
  let n = length us
  us' <- do
    i <- discrete $ discreteUniformAB 0 (n -1)
    u' <- random
    case splitAt i us of
      (xs, _ : ys) -> return $ xs ++ (u' : ys)
      _ -> error "impossible"
  ((b, q), vs) <- 
      runWriterT $ runWeighted 
      $ Weighted.hoist (WriterT . withPartialRandomness us') m
  let ratio = (exp . ln) $ min 1 
      (q * fromIntegral n / (p * fromIntegral (length vs)))
  accept <- bernoulli ratio
  return $ if accept then Trace vs b q else t
```

Our probabilistic program is interpreted in the type `Weighted (FreeSampler m) a`, which is an instance of `MonadInfer`. We use this to define our kernel on traces. We begin by perturbing the list of doubles contained in the trace by selecting a random position in the list and resampling there. We could do this *proposal* in a variety of ways, but here, we do so by choosing a double from the list at random and resampling it (hence, *single site* trace MCMC). We then run the program on this new list of doubles; `((b,q), vs)` is the outcome, probability, and result of all calls to `random`, respectively (recalling that the list of doubles may be shorter than the number of calls to `random`). The value of these is probabilistic in the underlying monad `m`. We then use the MH criterion to decide whether to accept the new list of doubles as our trace.

MH is then easily defined as taking steps with this kernel, in the usual fashion. Note that it works for any probabilistic program whatsoever.

## Implementations of inference methods

**PMMH**

```haskell=
pmmh t k n param model =
  mh t (param >>= runPopulation . pushEvidence . Pop.hoist lift . smcSystematic k n . model)
```

(Note that this uses the version of `mh` from `Control.Monad.Bayes.Traced.Static`.)

This is remarkably direct and simple given the complexity of the PMMH algorithm, but there's a lot to unpack here.

```haskell=
pmmh ::
  MonadInfer m =>
  -- | number of Metropolis-Hastings steps
  Int ->
  -- | number of time steps
  Int ->
  -- | number of particles
  Int ->
  -- | model parameters prior
  Traced m b ->
  -- | model
  (b -> Sequential (Population m) a) ->
  m [[(a, Log Double)]]
pmmh t k n param model =
  (mh t :: Traced m [(a, Log Double)] -> m [[(a, Log Double)]])
  ((param :: Traced m b) 
      >>= 
      (runPopulation :: Population (Traced m) a -> Traced m [(a, Log Double)]) 
      . (pushEvidence :: Population (Traced m) a -> Population (Traced m) a) 
      . Pop.hoist (lift :: forall x. m x -> Traced m x) 
      . (smcSystematic k n :: Sequential (Population m) a -> Population m a) 
      . (model :: b -> Sequential (Population m) a))
```

The idea is that the act of running a particle filter is itself traced. We then run MCMC to obtain a good run of this process.

"The idea is to do MH on the parameters of the model. Recall that for MH we need to compute the likelihood for the particular values of parameters but that involves integrating over the remaining random variables in the model which is intractable. Fortunately to obtain valid MH it is sufficient to have an unbiased estimator for the likelihood which is produced by a single sample from W. MH with such an estimator is referred to as pseudo-marginal MH. If instead of taking a single weight from W we take the sum of weights from Pop we obtain an unbiased estimator with lower variance. In particular if such a Pop is a result of smc the resulting algorithm is known as PMMH."

**Resample-Move Sequential Monte Carlo**

This is pretty complicated to think about, but again, the key is to think smart, not hard, and rely on the modularity of the architecture. 

"
A common problem with particle filters is that of particle degeneracy, where after resampling
many particles are the same, effectively reducing the sample size. One way to ameliorate this
problem is to introduce rejuvenation moves, where after each resampling we apply a number of
MCMC transitions to each particle independently, thus spreading them around the space. If we
use an MCMC kernel that preserves the target distribution at a given step, the resulting algorithm
is correct...
a program is interpreted
as a population of traced coroutines. It allows us to apply MH transitions to partially executed
coroutines, which is exactly what we require for the rejuvenation steps.
"

**Sequential Monte Carlo squared ($SMC^2$)**

todo

## Cheat Sheet


Basic instances of `MonadSample`:
- `SamplerIO`
- `Traced m`
- `Sequential m`

Basic instances of `MonadCond`:
- `Weighted m`
- `Population m`

Basic instances of `MonadInfer`:
- `Enumerator` (discrete distributions only)

Composed instances of `MonadInfer`:

- `Weighted SamplerIO` (used in weighted sampling)
- `Traced (Weighted SamplerIO)` (used in MCMC)
- `Sequential (Population (Weighted SamplerIO)` (used in SMC)
- todo: more



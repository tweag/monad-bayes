# Implementation guide

This document assumes the reader is familiar with the basics of Bayesian probability theory, basic Haskell (the syntax, the type system, do-notation, monad transformers), and how to specify distributions in monad-bayes (see the [docs](probprog.md))

That's enough to understand the core ideas, but for the more advanced content, you'll also want to feel comfortable enough with Haskell's type system that free monads, free monad transformers, and coroutines aren't a barrier to entry. And of course, to understand how inference methods like MCMC and SMC are implemented, it doesn't hurt to understand how they work in a statistical sense.

## References

Monad-Bayes is the codebase accompanying the theory of probabilistic programming described in [this paper](https://arxiv.org/pdf/1711.03219.pdf).

## The core typeclasses

The library relies on two core typeclasses `MonadDistribution` and `MonadFactor`. `MonadMeasure` is simply the union of the two, that is:

```haskell
(MonadDistribution m, MonadFactor m) => MonadMeasure m
```

You can find these in `Control.Monad.Bayes.Class`.

### MonadDistribution

Here is `MonadDistribution`:

```haskell
class Monad m => MonadDistribution m where
  random :: m Double
```

This one method, `random`, represents a uniform distribution over $[0,1]$. (`MonadDistribution` actually has a few other distributions, but that's not essential.)

What comes next is clever: you can define any other distribution you like in terms of `random`. As an example:

```haskell
bernoulli :: MonadDistribution m => m Bool
bernoulli p = fmap (< p) random
```

That one is pretty simple. As a more complex example, here's how a normal distribution is defined:

```haskell
normal m s = fmap (quantile (normalDistr m s)) random
```

`normalDistr` comes from a separate library `Statistics.Distribution.Normal` and `quantile (normalDistr m s) :: Double -> Double` is the inverse CDF of the normal, a deterministic function.

Again, to emphasize: **all of our randomness can be reduced to draws from a uniform distribution over the interval $[0,1]$**.

So we now have a way of constructing distributions in a monadic fashion. As a simple example:

```haskell
example :: MonadDistribution m => m Double
example = do
    x <- random
    y <- uniform 0 x
    return (x + y > 1.5)
```

Think of this as the procedure of first sampling uniformly from $[0,1]$, then from $[0,x]$, and then returning the Boolean $x + y > 1.5$. More precisely, this is the **marginal** probability of $x + y > 1.5$.

**Technical note**: `MonadDistribution` actually contains a number of other distributions beyond `random`, which by default are defined in terms of `random`, but allow for different definitions when desired. For example, `SamplerT` (an instance of `MonadDistribution` in Control.Monad.Sampler) defines `normal` and other distributions independently of `random`.

<!-- Inference is TODO
 `example1`, of type `MonadDistribution m => m Double`, and turn it into something we can actually see. Like samples. Or a parameter of a Bernoulli distribution. Those are problems for the next section, which is concerned with *interpreting* `MonadDistribution` as something more concrete, namely an inference algorithm. -->

<!-- The core philosophy of monad-bayes is that we specify distributions (probabilistic programs, that is) in this abstract monadic typeclass, and then cash it out in a variety of concrete ways which allow for convenient inference algorithms. -->

### MonadFactor

Here's `MonadFactor`.

```haskell
class Monad m => MonadFactor m where
  score :: Log Double -> m ()
```

`Log Double` is defined in `Numeric.Log`, and is a wrapper for `Double`s which does multiplication in log-space. It comes with `Exp :: a -> Log a` and `ln :: Log a -> a`. This is because we don't want numerical problems when multiplying tiny probabilities, and we want a hassle free typesafe separation of doubles and log-doubles.

<!-- `score` is less intuitive than `random`. It will become clearer when we discuss more concrete interpretations of `MonadFactor`, but what's important to know here is that it is used to allow us to do the Bayesian part of probability, as exemplified by the following probabilistic program:  -->



## Inference transformers

Now core idea of monad-bayes is that various monads will be made to be instances of `MonadDistribution`, `MonadFactor` or both (i.e. an instance of `MonadMeasure`), and different inference algorithms will be written using these instances. This separates the specification of the model (which happens abstractly in `MonadMeasure`) from the inference algorithm, which takes place in on of the concrete instances. The clever part of monad-bayes is that it allows this instances to be constructed in a modular way, using monad transformers. In the paper, these are termed *inference transformers* to emphasize that it doesn't really matter whether they satisfy the monad laws.

For example, to run weighted rejection sampling on a probabilistic program `p`, we can write `(sampler . runWeightedT) p`. Here, `(sampler . runWeightedT) :: WeightedT SamplerIO a -> IO a`. So `p` gets understood as being of type `WeightedT SamplerIO`, a type we'll encounter soon.

Some of these transformers are easy to understand (like `StateT Double`, while others (like the Church transformed Free monad transformer) lie on the more advanced side of things. The following tour of these types goes from easy to hard.

### Enumerator

Summary of key info:

- `Enumerator :: Type -> Type`
- `instance MonadDistribution Enumerator`
- `instance MonadFactor Enumerator`

`Enumerator` is in `Control.Monad.Bayes.Enumerator`, defined as follows:

```haskell
newtype Enumerator a =
    Enumerator (WriterT (Product (Log Double)) [] a)
```

This merits a little unpacking. First, `Product` is a wrapper from [Data.Monoid](https://hackage.haskell.org/package/base-4.16.0.0/docs/Data-Monoid.html) which makes the semigroup operator for numbers be multiplication, so that:

```haskell
Product 2 <> Product 3 == Product 6`
```

Unpacking the definition of `Enumerator a`, it is isomorphic to:

```haskell
[(a, (Product (Log Double)))]
```

So, a value of type `Enumerator Bool`, for instance, is a list of pairs of booleans along with a double, like:

```haskell
[(False,0.8914),(True,0.1086)]
```

Also in `Control.Monad.Bayes.Enumerator` is a function `enumerator`, which has type:

```haskell
enumerator :: Ord a => Enumerator a -> [(a, Double)]
```

We can write `enumerator sprinkler`. Why is this well typed? The idea is that `sprinkler` has type `forall m. MonadMeasure m => m Bool`, and we *instantiate* that `m` as `Enumerator`.

But for this to be well-typed, we need `Enumerator` to be an instance of `MonadMeasure`. For that, we need `Enumerator` to be a `MonadDistribution`, and a `MonadFactor`. For that, we need it to be a `Monad`, and in turn, a `Functor`. In understanding these instance definition, we'll understand what what `Enumerator` is doing for us.


`Enumerator` is a monad automatically, because `WriterT a m` is a monad for `a` a monoid and `m` a monad. As needed, `[]` is a monad and `Log Double` is a monoid. But what does that monad actually **do**?

For instance, if I have a weighted list `l` like `[(False,0.8914),(True,0.1086)]` and a function `f :: Bool -> Enumerator a`, what is `l >>= f`? It takes each element in `l`, and passes it through `f`, to obtain a weighted list of weighted lists. Then it flattens it, by including each element in the inner lists in the resulting list, with the weight of the list multiplied by the weight of the element. As for `return x`, it gives the singleton list containing the pair of `x` and the unit of the product monoid, i.e. `[(x, 1.0)]`.

This is the essence of propagating probability forward.

<!-- This is of course a representation of a discrete distribution, and as we might expect, `Enumerator` is only going to be useful when we are dealing with discrete distributions. -->

What remains is to define the `MonadDistribution` and `MonadFactor` instances:


```haskell
instance MonadDistribution Enumerator where
  random = error "Infinitely supported random variables not supported in Enumerator"
  bernoulli p = fromList [(True, (Exp . log) p), (False, (Exp . log) (1 - p))]


```

The first thing to notice is that `random` is actually not defined for `Enumerator`, and consequently, you can't handle any continuous distributions. This makes sense, because you can't represent continuous distributions (whose support is uncountably infinite) as a list of samples.

So really `Enumerator` isn't very general purpose. It's best understood as a didactic tool, rather than a real world inference algorithm.

But it can handle discrete distributions. It does this via `bernoulli` (as well as `categorical`, which I've omitted). `bernoulli p` constructs the weighted list corresponding to a `bernoulli` distribution with parameter `p`.

And the `MonadFactor` instance:

```haskell
instance MonadFactor Enumerator where
  score w = fromList [((), w)]
```

Finally, `enumerator` simply unwraps `Enumerator` to get a list of pairs of values and their weights, and then normalizes them into probabilities. It orders the list, which is why `enumerator` requires an `Ord` instance.

To see how this all works together, consider:

```haskell
example = do
  x <- bernoulli 0.5
  condition x
  return x
```

From the way the `MonadDistribution` instance for `Enumerator` is defined, `bernoulli 0.5` is a list of two pairs: `[(True, 0.5), (False, 0.5)]`. Using the `Monad` instance, the next line multiplies each of the masses by a number (`0` for `True`, `1` for `False`). The final line multiplies both by `1.0`. And then `enumerator` normalizes the result. So the ensuing distribution from `enumerator example` is `{True : 1.0}`.







<!-- ```haskell
example2 = do
    x <- bernoulli 0.5
    y <- bernoulli $ if x then 0.6 else 0.5
    return (y && x)
``` -->

### SamplerIO

Summary of key info on `SamplerIO`:

- `SamplerIO :: Type -> Type`
- `instance MonadDistribution SamplerIO`
- **No** instance for `MonadFactor`

Monad-Bayes actually provides a more general constructor:

```haskell
newtype SamplerT g m a = SamplerT (ReaderT g m a)
```

`SamplerIO` just specializes `g`, which is the random number generator, and `m`, the IO-handling monad:

```haskell
type SamplerIO = SamplerT (IOGenM StdGen) IO
```

But you can specialize many other ways (see the `random` package), depending on your use case.

As the names suggest, `SamplerIO` instantiates an abstract distribution as a sampling procedure. We have

```haskell
instance MonadDistribution (SamplerT g m) where
  random = SamplerT (ReaderT uniformDouble01M)

  uniform a b = SamplerT (ReaderT $ uniformRM (a, b))
  normal m s = SamplerT (ReaderT (MWC.normal m s))
  gamma shape scale = SamplerT (ReaderT $ MWC.gamma shape scale)
  beta a b = SamplerT (ReaderT $ MWC.beta a b)

  ...
```

Were the lines from `uniform` to `beta` commented out, they would be instantiated with the defaults defined in terms of `random` in the `MonadDistribution` class. The reason they are not is that there are more statistically efficient ways to sample.

We can unpack values from `SamplerIO a` using `sampler :: SamperIO a -> IO a`. So for example:


```haskell
print =<< sampler random
```

will print a sample from `random`. Similarly for other distributions.

But note that `SamplerIO` only has a `MonadDistribution` instance. This means that `sampler sprinkler` does not type check and gives the type error:

```haskell
No instance for (MonadMeasure SamplerIO)
```

This is to be expected. `SamplerIO` has no instance for `MonadFactor`.

### WeightedT m

Summary of key info on `WeightedT`:

- `WeightedT :: (Type -> Type) -> (Type -> Type)`
-  `instance MonadDistribution m => MonadMeasure (WeightedT m)`
- `instance MonadFactor (WeightedT m)`

```haskell
newtype WeightedT m a = WeightedT (StateT (Log Double) m a)
```

A **key** difference to `Enumerator` and `SamplerIO` is the **kind** of `WeightedT`: it takes a type `m :: Type -> Type` as an argument.


<!-- It takes not just a type `a :: Type` as argument, but also a type `m :: Type -> Type`. -->

`WeightedT m` is isomorphic to:

```haskell
Log Double -> m (a, Log Double)
```

Some intuition for what this means comes from the `MonadFactor` instance:

```haskell
instance Monad m => MonadFactor (WeightedT m) where
  score w = WeightedT (modify (* w))
```

So if we write:

```haskell
example :: MonadDistribution m => WeightedT m Bool
example = do
    x <- bernoulli 0.5
    score (if b then 1 else 0)
    return x
```

then the result is that first, we draw a sample from a Bernoulli distribution from the **underlying** distribution `m`, and then multiply the state (which is a `Log Double`) by a number which depends on that sample. For convenience, we write `condition b = score (if b then 1 else 0)`.

To unpack from `WeightedT m a`, we use:

```haskell
runWeightedT :: WeightedT m a -> m (a, Log Double)
runWeightedT (WeightedT m) = runStateT m 1
```

`WeightedT m` is not an instance of `MonadDistribution`, but only as instance of `MonadFactor` (and that, only when `m` is an instance of `Monad`). However, since `StateT` is a monad transformer, there is a function `lift :: m Double -> WeightedT m Double`.

So if we take a `MonadDistribution` instance like `SamplerIO`, then `Weighted SamplerIO` is an instance of both `MonadDistribution` and `MonadFactor`. Which means it is an instance of `MonadMeasure`.

So we can successfully write `(sampler . runWeightedT) sprinkler` and get a program of type `IO (Bool, Log Double)`. When run, this will draw a sample from `sprinkler` along with an **unnormalized** density for that sample.

It's worth stopping here to remark on what's going on. What has happened is that the `m` in `forall m. MonadMeasure m => m Bool` has been *instantiated* as `WeightedT SamplerIO`. This is an example of how the interpreters for inference can be composed in modular ways.

Finally, there's a function

```haskell
hoist :: (forall x. m x -> n x) -> WeightedT m a -> WeightedT n a
```

This takes a natural transformation `m ~> n` and lifts it into a natural transformation `WeightedT m ~> WeightedT n`. Most of the inference transformers have an analogous `hoist` function.


<!-- `WeightedT` and `SamplerIO` are on the simpler side, but illustrate the key principles of monad-bayes, namely:

**composable stack of inference transformers, implemented as instances of `MonadDistribution` and `MonadFactor` typeclasses.**

In a similar vein, `PopulationT` and `SequentialT` go together to handle Sequential Monte Carlo (SMC), and `TracedT` is associated with Markov Chain Monte Carlo (MCMC). -->



### PopulationT

Summary of key info on `PopulationT`:

- `PopulationT :: (Type -> Type) -> (Type -> Type)`
- `instance MonadDistribution m => instance MonadDistribution (PopulationT m)`
- `instance MonadFactor m => instance MonadFactor (PopulationT m)`

```haskell
newtype PopulationT m a = PopulationT (WeightedT (FreeT [] m) a)
```

The `FreeT []` construction is for branching our probabilistic program into different branches,
corresponding to different choices of a random variable.

It is interpreted, using `runPopulationT`, to:

```haskell
m [(a, Log Double)]
```

This shows that `Population` is used to compute a collection of particles (in the statistical sense), along with their weights.
Each `a` corresponds to one particle, and `Log Double` is the type of its weight.

There are several useful functions associated with it:

```haskell
spawn :: Monad m => Int -> PopulationT m ()
spawn n = fromWeightedList $ pure $ replicate n ((), 1 / fromIntegral n)
```

`spawn` spawns new particles. As an example:

```haskell
enumerator $ runPopulationT (spawn 2)
```

gives

```haskell
[([((),0.5),((),0.5)],1.0)]
```

Observe how here we have interpreted `(spawn 2)` as of type `Population Enumerator ()`.

`resampleGeneric` takes a function to probabilistically select a set of indices from a vector, and makes a new population by selecting those indices.

```haskell
resampleGeneric ::
  MonadDistribution m =>
    (V.Vector Double -> m [Int]) ->
    PopulationT m a ->
    PopulationT m a
```

`pushEvidence`, to quote the API docs, "normalizes the weights in the population, while at the same time incorporating the sum of the weights as a score in m."

```haskell
pushEvidence ::
  MonadFactor m =>
  PopulationT m a ->
  PopulationT m a
```

In other words, `pushEvidence` takes a `PopulationT m a` where `m` is a `MonadFactor` instance. It takes the sum of the weights, divides the weights by it, and then factors by the sum in `m`.

### SequentialT

Summary of key info on `SequentialT`:

- `SequentialT :: (Type -> Type) -> (Type -> Type)`
- `instance MonadDistribution m => instance MonadDistribution (SequentialT m)`
- `instance MonadFactor m => instance MonadFactor (SequentialT m)`


```haskell
newtype Sequential m a =
    Sequential {runSequential :: Coroutine (Await ()) m a}
```

This is a wrapper for the `Coroutine` type applied to the `Await` constructor from `Control.Monad.Coroutine`, which is defined thus:

```haskell
newtype Coroutine s m r = Coroutine {
   resume :: m (Either (s (Coroutine s m r)) r)
   }

newtype Await x y = Await (x -> y)
```

Unpacking that:

```haskell
Sequential m a ~ m (Either (() -> Sequential m a) a)
```

As usual, `m` is going to be some other probability monad, so understand `SequentialT m a` as representing a program which, after making a random choice or doing conditioning, we either obtain an `a` value, or a paused computation, which when resumed gets us back to a new `SequentialT m a`.

(For more on coroutines, see the final article in: https://themonadreader.files.wordpress.com/2011/10/issue19.pdf.)

The monad instance for coroutines is as follows:

```haskell
instance (Functor s, Monad m) => Monad (Coroutine s m) where
   return = pure
   t >>= f = Coroutine (resume t >>= apply f)
      where apply fc (Right x) = resume (fc x)
            apply fc (Left s) = return (Left (fmap (>>= fc) s))
```

The `MonadDistribution` instance follows from `lift : m a -> Coroutine (Await ()) m a`, defined as:

```haskell
instance Functor s => MonadTrans (Coroutine s) where
   lift = Coroutine . liftM Right
```


The `MonadFactor` instance has less trivial content:

```haskell
-- | Execution is 'suspend'ed after each 'score'.
instance MonadFactor m => MonadFactor (SequentialT m) where
  score w = lift (score w) >> suspend
```

First you take a `score` in the underlying `MonadFactor` instance, and then you `suspend`, which means:

```haskell
-- | A point where the computation is paused.
suspend :: Monad m => SequentialT m ()
suspend = SequentialT (Coroutine (return (Left (Await return))))
```
<!-- TODO: double check -->

We can move to the next suspension point with:

```haskell
advance :: Monad m => SequentialT m a -> SequentialT m a
advance = SequentialT . bounce extract . runSequentialT

```

and move through all with:

```haskell
-- | Remove the remaining suspension points.
finish :: Monad m => SequentialT m a -> m a
finish = pogoStick extract . runSequentialT
```

But most importantly, we can apply a natural transformation over the underlying monad `m` to only the current suspension.

```haskell
hoistFirst :: (forall x. m x -> m x) -> SequentialT m a -> SequentialT m a
hoistFirst f = SequentialT . Coroutine . f . resume . runSequentialT
```

When `m` is `PopulationT n` for some other `n`, then `resampleGeneric` gives us one example of the natural transformation we want. In other words, operating in `SequentialT (PopulationT n)` works, and not only works but does something statistically interesting: particle filtering (aka SMC).

**Note**: the running of `SequentialT`, i.e. getting from `SequentialT m a` to `m a` is surprisingly subtle, and there are many incorrect ways to do it, such as plain folds of the recursive structure. These can result in a semantics in which the transformation gets applied an exponentially large number of times.



### DensityT

Summary of key info on `DensityT`:

- `DensityT :: (Type -> Type) -> (Type -> Type)`
- `instance MonadDistribution (DensityT m)`
-  **No** instance for `MonadFactor`

A *trace* of a program of type `MonadDistribution m => m a` is an execution of the program, so a choice for each of the random values. Recall that `random` underlies all of the random values in a program, so a trace for a program is fully specified by a list of `Double`s, giving the value of each call to `random`.

With this in mind, a `DensityT m a` is an interpretation of a probabilistic program as a function from a trace to the *density* of that execution of the program.

Monad-bayes offers two implementations, in `Control.Monad.Bayes.DensityT.State` and `Control.Monad.Bayes.DensityT.Free`. The first is slow but easy to understand, the second is more sophisticated, but faster.

The former is relatively straightforward: the `MonadDistribution` instance implements `random` as `get`ting the trace (using `get` from `MonadState`), using (and removing) the first element (`put` from `MonadState`), and writing that element to the output (using `tell` from `MonadWriter`). If the trace is empty, the `random` from the underlying monad is used, but the result is still written with `tell`.

The latter is best understood if you're familiar with the standard use of a free monad to construct a domain specific language. For probability in particular, see this [blog post](https://jtobin.io/simple-probabilistic-programming). Here's the definition:

```haskell
newtype SamF a = Random (Double -> a)
newtype Density m a =
    Density {density :: FT SamF m a}

instance Monad m => MonadDistribution (Density m) where
  random = Density $ liftF (Random id)
```

The monad-bayes implementation uses a more efficient implementation of `FreeT`, namely `FT` from the `free` package, known as the *Church transformed Free monad*. This is a technique explained in https://begriffs.com/posts/2016-02-04-difference-lists-and-codennsity.html. But that only changes the operational semantics - performance aside, it works just the same as the standard `FreeT` datatype.

If you unpack the definition, you get:

```haskell
DensityT m a ~ m (Either a (Double -> (DensityT m a)))
```

Since `FreeT` is a transformer, we can use `lift` to get a `MonadDistribution` instance.

`runDensityT` is then defined using the folding pattern `iterFT`, which interprets `SamF` in the appropriate way:

```haskell
runDensityT :: MonadDistribution m => [Double] -> DensityT m a -> m (a, [Double])
runDensityT randomness (DensityT m) =
  runWriterT $ evalStateT (iterTM f $ hoistFT lift m) randomness
  where
    f (Random k) = do
      -- This block runs in StateT [Double] (WriterT [Double]) m.
      -- StateT propagates consumed randomness while WriterT records
      -- randomness used, whether old or new.
      xs <- get
      x <- case xs of
        [] -> random
        y : ys -> put ys >> return y
      tell [x]
      k x
```

This takes a list of `Double`s (a representation of a trace), and a probabilistic program like `example`, and gives back a `SamplerIO (Bool, [Double])`. At each call to `random` in `example`, the next double in the list is used. If the list of doubles runs out, calls are made to `random` using the underlying monad.

<!-- The intuition here is that given a list of doubles in $[0,1]$, you can evaluate any probabilistic program. If your list of numbers is shorter than the number of calls to `random` in the program, the remaining calls are made in the underlying `MonadDistribution` instance `m`.  -->


<!-- This uses `iterTM`, one of the rather expressive folding patterns in `Control.Monad.Trans.Free` (from the *free* package) and a clever use of the state monad transformer. The upshot is that you supply a  -->


### TracedT

Summary of key info on `TracedT`:

- `TracedT :: (Type -> Type) -> (Type -> Type)`
- `instance MonadDistribution m => MonadDistribution (TracedT m)`
- `instance MonadFactor m => MonadFactor (TracedT m)`

`TracedT m` is actually several related interpretations, each built on top of `DensityT`. These range in complexity.



<!-- Control flow is stochastic in a probabilistic program, which is to say that whether we draw from some variable may depend on a draw from some variable earlier. This means that we don't have a fixed structure like a Bayes net corresponding to a probabilistic program. -->

The reason traces are relevant here is that monad-bayes implements a version of Markov Chain Monte Carlo (MCMC) that operates on arbitrary probabilistic programs often referred to as *trace MCMC*. The idea is that the MCMC chain takes place on traces of the program. A step constitutes a change to this trace, i.e. to the list of `Double`s. For instance, the algorithm for an MH step goes as follows:

- propose a new trace by randomly redrawing a single `Double` in the trace
- accept or reject with the MH criterion

It's convenient to specify a trace not just as a `[Double]` but also with the resulting output, and the density of that output. This is what monad-bayes does:

```haskell
data Trace a = Trace
  {
    variables :: [Double],
    output :: a,
    runDensityT :: Log Double
  }
```

We also need a specification of the probabilistic program in question, free of any particular interpretation. That is precisely what `Density` is for.

The simplest version of `TracedT` is in `Control.Monad.Bayes.TracedT.Basic`

```haskell
TracedT m a ~ (DensityT Identity a, Log Double), m (Trace a))
```

A `TracedT` interpretation of a model is a particular run of the model with its corresponding probability, alongside a distribution over `Trace` info, which records: the value of each call to `random`, the value of the final output, and the density of this program trace.

This machinery allows us to implement MCMC (see inference methods below for details).

### Integrator

Summary of key info:

- `Integrator :: Type -> Type`
- `instance MonadDistribution Integrator`

`Integrator` is in `Control.Monad.Bayes.Integrator`, defined as follows:

```haskell
newtype Integrator a = Integrator {getCont :: Cont Double a}
```

This `MonadDistribution` instance interprets a probabilistic program as a numerical integrator. For a nice explanation, see [this blog post](https://jtobin.io/giry-monad-implementation).

`Integrator a` is isomorphic to `(a -> Double) -> Double`.
A program `model` of type `Integrator a` will take a function `f` and calculate $E_{p}[f] = \int f(x)*p(x)$ where $p$ is the density of `model`.

The integral for the expectation is performed by quadrature, using the tanh-sinh approach. For example, `random :: Integrator Double` is the program which takes a function `f` and integrates `f` over the $(0,1)$ range.

We can calculate the probability for an interval $(a,b)4 of any model of type `Integrator Double` by setting `f` to be the function that returns $1$ for that range, else $0$. Similarly for the CDF, MGF and so on.

## Inference methods under the hood

### Exact inference

Exact inference is nothing more than the use of the `Enumerator` instance of `MonadMeasure`. It should be noted that this is not a particularly efficient or clever version of exact inference.

For example, consider:

```haskell
example = replicateM 100 $ do
  x <- bernoulli 0.5
  condition x
  return x
```

Doing `enumerator example` will create a list of $2^{100}$ entries, all but one of which have $0$ mass. (See below for a way to perform this inference efficiently).

The main purpose of `Enumerator` is didactic, as a way to understand simple discrete distributions in full. In addition, you can use it in concert with transformers like `WeightedT`, to get a sense of how they work. For example, consider:

```haskell
example = do
  x <- bernoulli 0.5
  condition x
  return x
```

`(enumerator . weighted) example` gives `[((False,0.0),0.5),((True,1.0),0.5)]`. This is quite edifying for understanding `(sampler . weighted) example`. What it says is that there are precisely two ways the program will run, each with equal probability: either you get `False` with weight `0.0` or `True` with weight `1.0`.

### Quadrature

As described on the section on `Integrator`, we can interpret our probabilistic program of type `MonadDistribution m => m a` as having concrete type `Integrator a`. This views our program as an integrator, allowing us to calculate expectations, probabilities and so on via quadrature (i.e. numerical approximation of an integral).

This can also handle programs of type `MonadMeasure m => m a`, that is, programs with `factor` statements. For these cases, a function `normalize :: Weighted Integrator a -> Integrator a` is employed. For example,

```haskell
model :: MonadMeasure m => m Double
model = do
  var <- gamma 1 1
  n <- normal 0 (sqrt var)
  condition (n > 0)
  return var
```

is really an unnormalized measure, rather than a probability distribution. `normalize` views it as of type `Weighted Integrator Double`, which is isomorphic to `(Double -> (Double, Log Double) -> Double)`. This can be used to compute the normalization constant, and divide the integrator's output by it, all within `Integrator`.

### Independent forward sampling

For any program of type `p = MonadDistribution m => m a`, we may do `sampler p` or `runST $ sampleSTfixed p`. Note that if there are any calls to `factor` in the program, then it cannot have type `MonadDistribution m`.

### Independent weighted sampling

Consider

```haskell
example :: MonadMeasure m => m Bool
example = do
  x <- bernoulli 0.5
  condition x
  return x
```

`(runWeightedT . sampler) example :: IO (Bool, Log Double)` returns a tuple of a truth value and a probability mass (or more generally density). How does this work? Types are clarifying:

```haskell
run =
  (sampler :: SamplerIO (a, Log Double) -> IO (a, Log Double) )
  . (runWeightedT ::  WeightedT SamplerIO a -> SamplerIO (a, Log Double)
```

In other words, the program is being interpreted in the `WeightedT SamplerIO` instance of `MonadMeasure`.


### Metropolis Hastings MCMC

The version of MCMC in monad-bayes performs its random walk on program traces of type `Trace a`. The motivation for doing this is that **any** probabilistic program can then be used with MCMC entirely automatically.

A single step in this chain (in Metropolis Hasting MCMC) looks like this:

```haskell
mhTrans :: MonadDistribution m => (WeightedT (State.DensityT m)) a -> Trace a -> m (Trace a)
mhTrans m t@Trace {variables = us, probDensity = p} = do
  let n = length us
  us' <- do
    i <- discrete $ discreteUniformAB 0 (n - 1)
    u' <- random
    case splitAt i us of
      (xs, _ : ys) -> return $ xs ++ (u' : ys)
      _ -> error "impossible"
  ((b, q), vs) <- State.runDensityT (runWeightedT m) us'
  let ratio = (exp . ln) $ min 1 (q * fromIntegral n / (p * fromIntegral (length vs)))
  accept <- bernoulli ratio
  return $ if accept then Trace vs b q else t
```

Our probabilistic program is interpreted in the type `WeightedT (DensityT m) a`, which is an instance of `MonadMeasure`. We use this to define our kernel on traces. We begin by perturbing the list of doubles contained in the trace by selecting a random position in the list and resampling there. We could do this *proposal* in a variety of ways, but here, we do so by choosing a double from the list at random and resampling it (hence, *single site* trace MCMC). We then run the program on this new list of doubles; `((b,q), vs)` is the outcome, probability, and result of all calls to `random`, respectively (recalling that the list of doubles may be shorter than the number of calls to `random`). The value of these is probabilistic in the underlying monad `m`. We then use the MH criterion to decide whether to accept the new list of doubles as our trace.

MH is then easily defined as taking steps with this kernel, in the usual fashion. Note that it works for any probabilistic program whatsoever.

**Warning**: the default proposal is single site, meaning that you change one random number in the trace at a time. As a result, the proposal may not be ergodic for models with hard constraints. As a simple example, suppose that you're doing a random walk, and trying to get from a trace corresponding to the output `(True, True)` to a trace corresponding to `(False, False)`. But if `(False, True)` and `(True, False)` have no probability, you have no hope of getting there. **Don't expect MCMC to be efficient without designing your own proposal, or even correct when the proposal is not ergodic**.

### Sequential Importance Sampling

This is provided by

```haskell
sequentially ::
  Monad m =>
  -- | transformation
  (forall x. m x -> m x) ->
  -- | number of time steps
  Int ->
  SequentialT m a ->
  m a
sequentially f k = finish . composeCopies k (advance . hoistFirst f)
```

in `Control.Monad.Bayes.Sequential.Coroutine`. You provide a natural transformation in the underlying monad `m`, and `sequentially` applies that natural transformation at each point of conditioning in your program. The main use case is in defining `smc`, below, but here is a nice didactic use case:

Consider the program:

```haskell
example = replicateM 100 $ do
  x <- bernoulli 0.5
  condition x
  return x
```

Naive enumeration, as in `enumerator example` is enormously and needlessly inefficient, because it will create a $2^{100}$ size list of possible values. What we'd like to do is to throw away values of `x` that are `False` at each condition statement, rather than carrying them along forever.

Suppose we have a function `removeZeros :: Enumerator a -> Enumerator a`, which removes values of the distribution with $0$ mass from `Enumerator`. We can then write `enumerator $ sequentially removeZeros 100 $ model` to run `removeZeros` at each of the 100 `condition` statements, making the algorithm run quickly.

### Sequential Monte Carlo

Sequential importance resampling works by doing `sequentially` with a resampler of your choice, such as `resampleMultinomial`, after first spawning a set of `n` particles.

```haskell
smc ::
  Monad m =>
  -- | resampler
  (forall x. PopulationT m x -> PopulationT m x) ->
  -- | number of timesteps
  Int ->
  -- | population size
  Int ->
  -- | model
  SequentialT (PopulationT m) a ->
  PopulationT m a
smc resampler k n = sequentially resampler k . Seq.hoistFirst (spawn n >>)
```

### Particle Marginal Metropolis Hastings

Quoting the monad-bayes paper:

"PMMH is only applicable to models with a specific structure, namely the probabilistic program needs to decompose to a prior over the global parameters `m param` and the rest of the model `param -> m a`. Combining these using >>= would yield the complete model of type `m a`."

"The idea is to do MH on the parameters of the model. Recall that for MH we need to compute the likelihood for the particular values of parameters but that involves integrating over the remaining random variables in the model which is intractable. Fortunately to obtain valid MH it is sufficient to have an unbiased estimator for the likelihood which is produced by a single sample from `WeightedT`. MH with such an estimator is referred to as pseudo-marginal MH. If instead of taking a single weight from `WeightedT` we take the sum of weights from `PopulationT` we obtain an unbiased estimator with lower variance. In particular if such a `PopulationT` is a result of smc the resulting algorithm is known as PMMH."

Because of the modularity of monad-bayes, the implementation is remarkably simple, and directly linked to the algorithm:

```haskell
pmmh mcmcConf smcConf param model =
  mcmc
    mcmcConf
    ( param
        >>= runPopulationT
          . pushEvidence
          . Pop.hoist lift
          . smc smcConf
          . model
    )
```


There's a lot to unpack here. Here's the definition with more types. To shorten the signatures, the synonyms: `T = TracedT, S = SequentialT, P = PopulationT` are used:

```haskell
pmmh ::
  MonadDistribution m =>
  MCMCConfig ->
  SMCConfig (WeightedT m) ->
  TracedT (WeightedT m) a1 ->
  (a1 -> SequentialT (PopulationT (WeightedT m)) a2) ->
  m [[(a2, Log Double)]]
pmmh mcmcConf smcConf param model =
  (mcmc mcmcConf :: T m [(a, Log Double)] -> m [[(a, Log Double)]])
  ((param :: T m b) >>=
      (population :: P (T m) a -> T m [(a, Log Double)])
      . (pushEvidence :: P (T m) a -> P (T m) a)
      . Pop.hoist (lift :: forall x. m x -> T m x)
      . (smc smcConf :: S (P m) a -> P m a)
      . (model :: b -> S (P m) a))
```

(Note that this uses the version of `mcmc` that uses the `TracedT` representation from `Control.Monad.Bayes.TracedT.Static`.)

To understand this, note that the outer algorithm is just `mcmc`. But the probabilistic program that we pass to `mcmc` does the following: run `param` to get values for the parameters and then pass these to `model`. Then run `n` steps of SMC with `k` particles. Then lift the underlying monad `m` into `TracedT m`. Then calculate the sum of the weights of the particles and `factor` on this (this is what `pushEvidence` does). This `factor` takes place in `TracedT m`, so it is "visible" to `mcmc`.


### Resample-Move Sequential Monte Carlo

<!-- This is pretty complicated to think about, but again, the key is to think smart, not hard, and rely on the modularity of the architecture.  -->

The paper introducing monad-bayes has this to say about resample-move SMC:

"A common problem with particle filters is that of particle degeneracy, where after resampling many particles are the same, effectively reducing the sample size. One way to ameliorate this problem is to introduce rejuvenation moves, where after each resampling we apply a number of MCMC transitions to each particle independently, thus spreading them around the space. If we use an MCMC kernel that preserves the target distribution at a given step, the resulting algorithm is correct"

monad-bayes provides three versions of RMSMC, each of which uses one of the three `TracedT` implementations respectively. Here is the simplest, which I have annotated with types. To shorten the signatures, the synonyms: `T = TracedT, S = SequentialT, P = PopulationT` are used:

```haskell

rmsmcBasic ::
  MonadDistribution m =>
  MCMCConfig ->
  SMCConfig m ->
  -- | model
  S (T (P m)) a ->
  P m a
rmsmc (MCMCConfig {..}) (SMCConfig {..}) =
  (TrBas.marginal :: T (P m) a -> P m a )
  . sequentially
      (
      (composeCopies numMCMCSteps TrBas.mhStep :: T (P m) a -> T (P m) a )
      . TrBas.hoist (resampleSystematic :: P m a -> P m a ) )
      numSteps
  . (S.hoistFirst (TrBas.hoist (spawn numParticles >>)) :: S (T (P m)) a -> S (T (P m)) a ))
```

What is this doing? Recall that `S m a` represents an `m` of coroutines over `a`. Recall that `TracedT m a` represents an `m` of  traced computations of `a`. Recall that `P m a` represents an `m` of a list of `a`s.

This means that an `S (T (P m)) a` is a program "interpreted as a population of traced coroutines". The paper adds that this "allows us to apply MH transitions to partially executed coroutines, which is exactly what we require for the rejuvenation steps."

So the algorithm works by creating `n` particles, and at each of the first `k` calls to `factor`, first resampling the population and then for each particle in the population, doing an MH-MCMC walk for `t` steps to update it.

### Sequential Monte Carlo Squared

This combines RMSMC and PMMH. That is, it is RMSMC, but for the MCMC rejuvenation procedure, PMMH is used instead of MH.

There is one slight complication here, related to the fact that the MTL style effect system approach requires newtypes when more than one of a given effect appears in a stack, in order to differentiate them.

<!-- todo: finish -->

<!-- ## Cheat Sheet


Basic instances of `MonadDistribution`:
- `SamplerIO`
- `TracedT m`
- `SequentialT m`

Basic instances of `MonadFactor`:
- `WeightedT m`
- `SequentialT m`

Basic instances of `MonadMeasure`:
- `Enumerator` (discrete distributions only)

Composed instances of `MonadMeasure`:

- `WeightedT SamplerIO` (used in weighted sampling)
- `TracedT (WeightedT SamplerIO)` (used in MCMC)
- `SequentialT (PopulationT (WeightedT SamplerIO)` (used in SMC)
-  more -->

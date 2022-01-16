# User Guide

Probabilistic programming is all about being able to write programs like:

```haskell
sprinkler :: MonadInfer m => m Bool
sprinkler = do
  rain <- bernoulli 0.3
  sprinkler <- bernoulli $ if rain then 0.1 else 0.4
  wet <- bernoulli $ case (rain, sprinkler) of
    (True, True) -> 0.98
    (True, False) -> 0.8
    (False, True) -> 0.9
    (False, False) -> 0.0
  condition (not wet)
  return rain
```

and doing then do inference like this:

```haskell   
enumerate sprinkler
```

to get

```
[(False,0.8914),(True,0.1086)]
```

`sprinkler` is a distribution over values for the Boolean `rain` variable given the likelihood and observation specified above. `enumerate` is a function which performs **inference**: it takes the abstract distribution `sprinkler` and calculates something concrete - in this case, the probability mass function.

`sprinkler` is a distribution specified as a program with randomness (e.g. `bernoulli`) and scoring (e.g. `condition`). Hence: probabilistic programming. The Grand Vision is that you write your statistical model as a probabilistic program and then choose or construct a method to perform inference in a statistically and computationally efficient way.

## monad-bayes vs other libraries

monad-bayes is a universal probabilistic programming language, in the sense that you can express any computable distribution. In this respect it differs from Stan, which focuses instead on handling an important subset well.

There is a variety of universal probabilistic programming libraries and/or languages, which include WebPPL, Gen, Pyro and Edward.

Advantage of these other libraries/languages:

- a lot of engineering work has been put into these libraries and languages to make them practical for real-world problems. The same is not true for monad-bayes. While its core is very nice, it doesn't come with a lot of the batteries you might want. Adam Scibior's thesis contains this relevant paragraph: "our library implements basic versions of advanced sampling algorithms. However, their successful application in practice requires incorporating established heuristics, such as: adaptive proposal distributions, controlling resampling with effective sample size, tuning rejuvenation kernels based on population in SMC2, and so on. We believe these are largely orthogonal to the core design, so excluding them makes for a clearer and more accessible presentation of the main ideas."

Advantages of monad-bayes: 

- probabilistic programs in monad-bayes are first class programs in Haskell. This allows all of Haskell's expressive power to be brought to bear. You can write distributions over any datatype (lists, trees, functions, etc). You can use powerful libraries like Pipes, lens and Parsec. Everything is totally pure. You can make use of laziness. The types are invaluable. There's no new special syntax to learn, which makes use and development much easier.

- inference algorithms are modular. Complex inference algorithms like RMSMC or PMMH are built out of simple composable pieces.

## References

Other probabilistic programming languages with fairly similar APIs include WebPPL and Gen. This cognitive-science oriented introduction to WebPPL is an excellent resource for learning about probabilistic programming: https://probmods.org/. The tutorials for Gen are also very good, particularly for learning about traces: https://github.com/probcomp/gen-quickstart/blob/master/tutorials/A%20Bottom-Up%20Introduction%20to%20Gen.ipynb.

## Specifying distributions

A distribution in monad-bayes over a set $X$, is of type:

```haskell
MonadInfer m => m X
```

monad-bayes provides standard distributions, such as:

- `random :: MonadInfer m => m Double` : sample uniformly from $[0,1]$

The full set is listed at https://hackage.haskell.org/package/monad-bayes-0.1.1.0/docs/Control-Monad-Bayes-Class.html

Note that these primitives already allows us to construct quite exotic distributions, like the uniform distribution over `(+) :: Int -> Int -> Int` and `(-) :: Int -> Int -> Int`:

```haskell
distributionOverFunctions = uniformD [(+), (-)]
```

### Constructing distributions as programs

monad-bayes also lets us construct new distributions out of these. `MonadInfer m` implies `Monad m` and in turn `Functor m`, so we can do the following:

```haskell
fmap (> 0.5) random :: MonadInfer m => m Bool
```

This is the uniform distribution over $(0.5, 1]$.

As an important special case, if `x :: MonadInfer m => m (a,b)` is a joint distribution over two variables, then `fmap fst a :: MonadInfer m => m a` **marginalizes** out the second variable. That is to say, `fmap fst a` is the distribution $p(a)$, where $p(a) = \int_b p(a,b)$

The above example use only the functor instance for `m`, but we also have the monad instance, as used in:

```haskell
example :: MonadInfer m => m Double
example = bernoulli 0.5 >>= (\x -> if x then random else normal 0 1)
```

It's easiest to understand this distribution as a probabilistic program: it's the distribution you get by first sampling from `bernoulli 0.5`, then checking the result. If the result is `True`, then sample from `random`, else from `normal 0 1`. As a distribution, this has a PDF:

$$ f(x) = 1[0\leq x \leq 1]*0.5  + \mathcal{N}(0,1)(x)*0.5  $$
<!-- $$ \int\_{[0,1]} 1[x>0.5]* + (1[x\leq 0.5]*N(0,1)(x)) dx $$ -->

Equivalently, we could write this in do-notation as:

```haskell
example :: MonadInfer m => m Double
example = do
  bool <- bernoulli 0.5
  if bool then random else normal 0 1
```

**A technical note**: it is often tempting to read the line `bool <- bernoulli 0.5` as saying "take a sample from `bernoulli 0.5`. But although we'll see below that `example` can be interpreted as a sampler, there are many other interpretations, not least as a mathematical specification of a particular distribution.

That said, it is often useful to think of probabilistic programs as specifying distributions over **program executation traces**. For example, one trace of `example` as defined above is (informally): `{bernoulli 0.5 : True, random : 0.7}`.

### Hard and soft conditioning

monad-bayes provides a function `score :: MonadInfer m => Log Double -> m ()`. (**Note**: `Log Double` is a wrapper for `Double` which stores doubles as their logarithm, and does multiplication by addition of logarithms.)

```haskell
example :: MonadInfer m => m Double
example = do
  bool <- bernoulli 0.5
  number <- if bool then random else normal 0 1
  score number 
  return bool
```

It's easiest to understand this in terms of the "program execution trace" perspective described above. What the score statement does is to multiple every trace by the value of `number` in that particular trace.

`condition` in then defined as follows:

```haskell
condition :: MonadInfer m => Bool -> m ()
condition b = score $ if b then 1 else 0
```
So `condition b` throws away every trace in which `b` is False, and keeps all traces in which `b` is True. For example:

```haskell
example :: MonadInfer m => m Int
example = do
  n <- poisson 0.5
  condition (n%2 == 0)
  return n
```

This describes a poisson distribution in which all even values of the random variable are marginalized out.

<!-- Another use case is Bayesian inference as in:

<!-- The most intuitive way to understand `score` is to think of a probabilistic program as making a series of random choices which trace out a possible execution of the program. At any point in this series, we can interject a `score x` statement, where the value of `x` depends on the previous choices. This statement multiplies the weight of this "trace" by the score. -->

<!-- ```haskell
bayesianExample :: (Eq a, MonadInfer m) => m a -> (a -> m b) -> (b -> m a)
bayesianExample prior likelihood b = do
    a <- prior
    b' <- likelihood a
    condition (b==b')
    return a
```

Note that operationally speaking, this approach is only going to work well for discrete distributions, since `b==b'` is going to be zero-measure in the continuous case. But in the discrete case, we could for example do: -->

<!-- ```haskell
example :: MonadInfer 
example =  bayesianExample (bernoulli 0.5) (\x -> if x then bernoulli 0.8 else bernoulli 0.9) True 
``` 
-->




<!-- ```haskell
example :: MonadInfer m => m Bool
example = do 
  x <- normal 0 1
  y <- normal 0 2
  z <- normal 0 3
  return (x > y)
```

Note that in this example, commenting out the line `z <- normal 0 3` would not change the distribution at all. **But**, there is no guarantee in theory that the inference method you use knows this. More generally,  -->

<!-- **Not all ways of expressing denotationally equivalent distributions are equally useful in practice** -->

## Performing inference

To quote [this page](https://webppl.readthedocs.io/en/master/inference/), "marginal inference (or just inference) is the process of reifying the distribution on return values implicitly represented by a stochastic computation.". That is, a probabilistic program (stochastic computation) is an abstract object and inference transforms it into something concrete, like a histogram, a list of samples, or parameters of a known distribution.

All inference methods in monad-bayes work with all distributions. The only exception is that exact inference only works with discrete distributions and will throw a runtime error on continuous distributions.

**The challenge of inference** is that most distributions that are of interest are not as simple as `sprinkler`. They could have continuous random variables, a huge number of them, or even a number of them that is itself random. They could involve a series of observations, interspersed with other sources of randomness.

Designing a language in which you can specify arbitrarily complex (computable) distributions as probabilistic programs turns out to be a largely solved problem. The tools given about are sufficient for that. 

The hard part is designing a language where you can specify how you want to do inference, because sophisticated, often approximate, inference methods are almost always necessary for the models involved in solving real world problems.

Two of the large classes of inference methods are **sampling based methods** and **gradient based methods**. The latter only apply to continuous probability distributions, and are not the focus of monad-bayes.

<!-- For the purposes of this section, let `dist :: MonadInfer m => m a` be the distribution you want to perform inference on.  -->

### Exact inference

```haskell
enumerate :: Ord a => Enumerator a -> [(a, Double)]
```

So `enumerate (bernoulli 0.7)` gives you

```
[(False,0.3),(True,0.7)]
```

**Note: enumerate only works on finite discrete distributions**

It will run forever on infinite distributions like `enumerate (poisson 0.7)` and will throw the following **runtime** error on continuous distributions as in `enumerate (normal 0 1)`:

*"Exception: Infinitely supported random variables not supported in Enumerator"*

**However**, it's totally fine to have the elements of the support be an instance of `Ord`, as in:

```haskell
fmap (\(ls,p) -> (take 4 ls, p)) $ enumerate $ uniformD [[1..], [2..]]
```

which gives

```
[([1,2,3,4],0.5),([2,3,4,5],0.5)]
```

### Independent forward sampling

For any probabilistic program `p` without any `condition` or `factor` statements, we may do `sampleIO p` or `sampleSTfixed p` (to run with a fixed seed) to obtain a sample in an ancestral fashion. For example, consider:

```haskell
example = do
  x <- bernoulli 0.5
  if x then normal 0 1 else normal 1 2
```

`sampleIO example` will produce a sample from a Bernoulli distribution with $p=0.5$, and if it is $True$, return a sample from a standard normal, else from a normal with mean 1 and std 2. '

`(replicateM n . sampleIO) example` will produce a list of `n` independent samples. However, it is recommended to instead do `(sampleIO . replicateM n) example`, which will create a new model (`replicateM n example`) consisting of `n` independent draws from `example`. 

Because `sampleIO example` is totally pure, it is parallelizable. 

### Independent weighted sampling

To perform weighted sampling, use:

```haskell
(sampleIO . runWeighted) :: Weighted SamplerIO a -> IO (a, Log Double)
```

`Weighted SamplerIO` is an instance of `MonadInfer`, so we can apply this to any distribution. For example, suppose we have the distribution:

```haskell
example :: MonadInfer m => m Bool
example = do
  x <- bernoulli 0.5
  condition x
  return x
```

Then:

```haskell
run :: IO (Bool, Log Double)
run = (sampleIO . runWeighted) example
```

is an IO operation which when run, will display either `(False, 0.0)` or `(True, 1.0)`


### Markov Chain Monte Carlo

There are several versions of metropolis hastings MCMC defined in monad-bayes. The standard version is found in Control.Monad.Bayes.Traced. You can use it as follows:

```haskell
(sampleIO . prior . mh numSteps) :: Traced (Weighted SamplerIO) a -> IO [a]
```

`Traced (Weighted SamplerIO)` is an instance of `MonadInfer`, so we can apply this to any distribution. For instance:


```haskell
example :: MonadInfer m => m Bool
example = do
  x <- bernoulli 0.5
  condition x
  return x
```

Then 

```haskell
run :: IO [Bool]
run = (sampleIO . prior . mh 10) example
```

produces 10 unbiased samples from the posterior, by using single-site trace MCMC with the Metropolis-Hastings (MH) method. This means that the random walk is over execution traces of the probabilistic program, and the proposal distribution modifies a single random variable as a time, and then uses MH for the accept-reject criterion. For example, from the above you'd get:

```
[True,True,True,True,True,True,True,True,True,True,True]
```

The end of the chain is the head of the list, so you can drop samples from the end of the list for burn-in.


### Sequential Monte Carlo (Particle Filtering)

```haskell
(sampleIO. runPopulation. smcSystematic numSteps numParticles) 
  :: Sequential (Population SamplerIO) a -> IO [(a, Numeric.Log.Log Double)]
```

`Sequential (Population SamplerIO)` is an instance of `MonadInfer`, so we can apply this to any distribution. For instance, to use our now familiar `example`:


```haskell
example :: MonadInfer m => m Bool
example = do
  x <- bernoulli 0.5
  condition x
  return x
```

Then 

```haskell
run :: IO [Bool]
run = (sampleIO . runPopulation. smcSystematic 4 4) example
```

```
[(True,6.25e-2),(True,6.25e-2),(True,6.25e-2),(True,6.25e-2)]
```

Each of these is a particle with a weight.


### Resample Move Sequential Monte Carlo

<!-- todo -->

### Particle Marginal Metropolis Hastings

This inference method takes a prior and a model separately.


<!-- todo -->

<!-- Here I use "inference" to mean the process of getting from the distribution in the abstract the something concrete, like samples from it,  an expectation over it, parameters of it, or in the above case of `enumerate`, the mass of each element of the support. -->


<!-- You then want to be able to convert this abstract specification of a distribution or model into something tangible, and in the case of this simple discrete distribution, we can do so by brute force. That's what `enumerate` does. -->




<!-- It feels natural that a pure, functional, strongly typed language like Haskell should have a good story for Bayesian probability, inference, and probabilistic programming.  -->





<!-- denotation of probabilistic programs, which we then are free to interpret in myriad ways: as weighted lists, samplers, or a variety of more sophisticated programs for performing inference -->

<!-- In particular, these interpretations can be combined, and by so doing, you can built up really rather complex inference algorithms while being sure that your method is sound. And also intelligible.  -->

<!-- *The interpretation of your model is the program which performs inference on it* -->

# Example Gallery

<!-- todo: link to monad-bayes examples, with graphs and how to run -->

<!-- `sprinkler` above is a great example of the two new things you can do in a probabilistic program that you can't do in other programs: you can draw from distributions, and you can *condition* on observations. For example:

```haskell
example = do
    ind <- fmap not (bernoulli 0.9)
    val <- if ind then gaussian 0 1 else poisson 0.5
    condition (val > 1)
    return ind
```

This example is contrived, in order to show a few things. First, `m` in distributions like `bernoulli 0.9 :: MonadSample m => m Bool` (the distribution `{True : 0.9, False: 0.1}`) are functors, so we can fmap over them, e.g. with `not` to get the distribution `{True : 0.1, False: 0.9}`. Second, distributions are monads, so we can draw from them and use the results as the parameters of other distributions. Third, we have a `condition` function, which throws out all values of `ind` which would result in `val <= 1`.

The fact that distributions are a monad is the essence of probabilistic programming. It allows you to express everything from simple models (Bayesian linear regression) to complex ones (hierarchical latent Dirichlet models) in a shared language. See the `models` folder (TODO LINK) for examples.

```haskell
betaBernoulli :: MonadSample m => Int -> m [Bool]
betaBernoulli n = do
  weight <- uniform 0 1
  let toss = bernoulli weight
  replicateM n toss
```

 -->

## Interoperating with other Haskell code

Probabilistic programs in monad-bayes are Haskell programs. This contrasts to many probabilistic programming languages, which are deeply embedded and cannot smoothly interact with their host language. 

For example, we can use ordinary monadic combinators, as in:

```haskell
example = do
  x <- bernoulli 0.5
  when x (score 0.8)
  return x
```

or 

```haskell
example = whileM (bernoulli 0.99) (normal 0 1)
```

We can use libraries like Pipes, to specify lazy distributions as in models/Pipes.hs

We can write probabilistic optics to update or view latent variables, as in models/Lens.hs.

We can define models like PCFGs using recursion schemes, as in models/PCFG.hs.

We can write probabilistic parsers, as in models/Parser.hs.

We can use monad transformers on top of our probability monad `m`, as in models/Failure.hs.

<!-- And, because we're programming directly in Haskell, rather than a domain specific language (like Church, Gen, WebPPL and most other probabilistic programming languages), we can interoperate with any other Haskell concepts. Two examples: -->


## API docs

For API docs, see [hackage](https://hackage.haskell.org/package/monad-bayes).

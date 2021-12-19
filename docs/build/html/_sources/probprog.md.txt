# User Guide

Probabilistic programming is all about being able to write programs like:

```haskell=
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

`sprinkler` is a distribution over values for the Boolean `rain` variable given the likelihood and observation specified above.

`sprinkler` is a distribution. But it's a distribution specified as a program with randomness (e.g. `bernoulli`) and scoring (e.g. `condition`). Hence: probabilistic programming. The Grand Vision is that you write your statistical model as a probabilistic program and then choose or construct a method to perform inference in a statistically and computationally efficient way.

## Specifying distributions



A distribution in monad-bayes over a set $X$, is of type:

```haskell=
MonadInfer m => m X
```
### Basic distributions

monad-bayes provides these basic distributions:

- `random :: MonadInfer m => m Double` : sample uniformly from $[0,1]$
TODO: more distributions

### Constructing distributions monadically

monad-bayes also lets us construct new distributions out of these. `MonadInfer` instances are also `Functor` and `Monad` instances, so we can do the following:

```haskell=
fmap (> 0.5) random :: MonadInfer m => m Bool
```

This is the distribution over TODO

As an important special case, if `x :: MonadInfer m => m (a,b)` is a joint distribution over two variables, then `fmap fst a` marginalized out the second variable.

```haskell=
bernoulli 0.5 >>= (\x -> if x then random else normal 0 1)
```

It's easiest to understand this distribution as a program: it's the distribution you get by first sampling from `bernoulli 0.5`, then checking the result. If the result is `True`, then sample from `random`, else from `normal 0 1`. As a distribution, this has a PDF:

$$ \int\_{[0,1]} 1[x>0.5]* + (1[x\leq 0.5]*N(0,1)(x)) dx $$

We can also use do-notation, as in:

sprinkler

## Performing inference

To quote [this page](https://webppl.readthedocs.io/en/master/inference/), "marginal inference (or just inference) is the process of reifying the distribution on return values implicitly represented by a stochastic computation.". That is, a probabilistic program (stochastic computation) is an abstract object and inference transforms it into something concrete, like a histogram, a list of samples, or parameters of a known distribution.

All inference methods in monad-bayes work with all distributions. The only exception is that exact inference only works with discrete distributions and will throw a runtime error on continuous distributions.

**The challenge of inference** is that most distributions that are of interest are not as simple as `sprinkler`. They could have continuous random variables, a huge number of them, or even a number of them that is itself random. They could involve a series of observations, interspersed with other sources of randomness.

Designing a language in which you can specify arbitrarily complex models as probabilistic programs turns out to be a largely solved problem. The hard part is designing a language where you can specify how you want to do inference, because sophisticated, often approximate, inference methods are almost always necessary for the models involved in solving real world problems.

Two of the large classes of inference methods are **sampling based methods** and **gradient based methods**. The latter only apply to continuous probability distributions, and are not the focus of monad-bayes.

For the purposes of this section, let `dist :: MonadInfer m => m a` be the distribution you want to perform inference on. 

### Exact inference

```haskell=
enumerate :: Ord a => Enumerator a -> [(a, Double)]
```

So `enumerate dist` gives you each element in the support of `dist`, and its mass.

**Only works on finite discrete distributions**


things you can do:
    distribution of functions
    distribution of unshowable values 

### Weighted sampling

(sampleIO . runWeighted) 

### Markov Chain Monte Carlo

sampleIO . prior . mh n

### Sequential Monte Carlo (Particle Filtering)

smc

### Particle Marginal Metropolis Hastings

### Resample Move Sequential Monte Carlo

<!-- " -->

<!-- Here I use "inference" to mean the process of getting from the distribution in the abstract the something concrete, like samples from it,  an expectation over it, parameters of it, or in the above case of `enumerate`, the mass of each element of the support. -->


<!-- You then want to be able to convert this abstract specification of a distribution or model into something tangible, and in the case of this simple discrete distribution, we can do so by brute force. That's what `enumerate` does. -->




<!-- It feels natural that a pure, functional, strongly typed language like Haskell should have a good story for Bayesian probability, inference, and probabilistic programming.  -->





<!-- denotation of probabilistic programs, which we then are free to interpret in myriad ways: as weighted lists, samplers, or a variety of more sophisticated programs for performing inference -->

<!-- In particular, these interpretations can be combined, and by so doing, you can built up really rather complex inference algorithms while being sure that your method is sound. And also intelligible.  -->

<!-- *The interpretation of your model is the program which performs inference on it* -->

# Example Gallery


<!-- `sprinkler` above is a great example of the two new things you can do in a probabilistic program that you can't do in other programs: you can draw from distributions, and you can *condition* on observations. For example:

```haskell=
example = do
    ind <- fmap not (bernoulli 0.9)
    val <- if ind then gaussian 0 1 else poisson 0.5
    condition (val > 1)
    return ind
```

This example is contrived, in order to show a few things. First, `m` in distributions like `bernoulli 0.9 :: MonadSample m => m Bool` (the distribution `{True : 0.9, False: 0.1}`) are functors, so we can fmap over them, e.g. with `not` to get the distribution `{True : 0.1, False: 0.9}`. Second, distributions are monads, so we can draw from them and use the results as the parameters of other distributions. Third, we have a `condition` function, which throws out all values of `ind` which would result in `val <= 1`.

The fact that distributions are a monad is the essence of probabilistic programming. It allows you to express everything from simple models (Bayesian linear regression) to complex ones (hierarchical latent Dirichlet models) in a shared language. See the `models` folder (TODO LINK) for examples.

```haskell=
betaBernoulli :: MonadSample m => Int -> m [Bool]
betaBernoulli n = do
  weight <- uniform 0 1
  let toss = bernoulli weight
  replicateM n toss
```

 -->

## Interoperating with other Haskell code

Probabilistic programs in monad-bayes are Haskell programs. This constrasts to many probabilistic programming languages, which are deeply embedded and cannot smoothly interact with their host language. 

<!-- And, because we're programming directly in Haskell, rather than a domain specific language (like Church, Gen, WebPPL and most other probabilistic programming languages), we can interoperate with any other Haskell concepts. Two examples: -->


```haskell=
model :: (MonadError String m, MonadInfer m) => m Int
model = do
    x <- uniformD [0..10]
    conditionAllowingForFailure (x < 0)
    return x

conditionAllowingForFailure :: (MonadSample m, MonadError [Char] m, MonadCond m) 
    => Bool -> m ()
conditionAllowingForFailure b = do 
    fail <- bernoulli 0.1
    when fail $ throwError "fail"
    condition b

main :: [(Either String Int, Double)]
main = enumerate $ runExceptT model
-- >>> main
-- [(Left "fail",1.0),(Right 0,0.0),(Right 1,0.0),(Right 2,0.0),(Right 3,0.0),(Right 4,0.0),(Right 5,0.0),(Right 6,0.0),(Right 7,0.0),(Right 8,0.0),(Right 9,0.0),(Right 10,0.0)]
```

Here, I've added `ExceptT` to my probability monad, which allows me to throw errors. The result is that I can write a version of `condition` which results in a safe failure when I condition on something that is always false. 

```haskell=
recursion scheme example: pcfg
```

## API docs

For API docs, see [hackage](https://hackage.haskell.org/package/monad-bayes).

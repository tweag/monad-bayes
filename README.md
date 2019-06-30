# monad-bayes

[![Build Status](https://travis-ci.org/adscib/monad-bayes.svg?branch=master)](https://travis-ci.org/adscib/monad-bayes)

A library for probabilistic programming in Haskell using probability monads. The emphasis is on composition of inference algorithms implemented in terms of monad transformers. The code is still experimental, but will be released on Hackage as soon as it reaches relative stability. User's guide will appear soon. In the meantime see the `models` folder that contains several examples.

The code in this repository accompanies the ICFP 2018 paper [Functional programming for modular Bayesian inference](https://dl.acm.org/citation.cfm?id=3236778).

For the code associated with Haskell2015 paper "Practical Probabilistic Programming with Monads" see the `haskell2015` branch.

## Installation (using Stack)

Ensure stack is already installed by following these [instructions](https://docs.haskellstack.org/en/stable/install_and_upgrade/)

To fork repo:

```
git clone https://github.com/adscib/monad-bayes.git
```

To run the build:

```
stack build
```

To test the code:

```
stack test
```

To open interactive session:

```
stack ghci
```


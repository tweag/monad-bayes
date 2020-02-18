# monad-bayes

[![Hackage](https://img.shields.io/hackage/v/monad-bayes.svg)](https://hackage.haskell.org/package/monad-bayes) [![Hackage Deps](https://img.shields.io/hackage-deps/v/monad-bayes.svg)](http://packdeps.haskellers.com/reverse/monad-bayes)

A library for **probabilistic programming in Haskell** using probability
monads. The emphasis is on composition of inference algorithms implemented in
terms of monad transformers.

Created by [Adam Scibior][adam-web] ([@adscib][adam-github]), maintained by
[Tweag I/O][tweagio].

## Project status

Now that `monad-bayes` has been released on Hackage, we will focus on improving
documentation. In the meantime, see the [`models` folder][models] for examples.

## Background

The basis for the code in this repository is the ICFP 2018 paper [2]. For the
code associated with the Haskell2015 paper [1], see the [`haskell2015`
branch][haskell2015-branch].

[1] Adam M. Ścibior, Zoubin Ghahramani, and Andrew D. Gordon. 2015. [Practical
probabilistic programming with monads][haskell2015-doi]. In _Proceedings of the
2015 ACM SIGPLAN Symposium on Haskell_ (Haskell ’15), Association for Computing
Machinery, Vancouver, BC, Canada, 165–176.

[2] Adam M. Ścibior, Ohad Kammar, and Zoubin Ghahramani. 2018. [Functional
programming for modular Bayesian inference][icfp2018-doi]. In _Proceedings of
the ACM on Programming Languages_ Volume 2, ICFP (July 2018), 83:1–83:29.

[3] Adam M. Ścibior. 2019. [Formally justified and modular Bayesian inference
for probabilistic programs][thesis-doi]. Thesis. University of Cambridge.

## Installation (using Stack)

Ensure `stack` is installed by following these [instructions][stack-install].

To clone the repo:

```
git clone https://github.com/tweag/monad-bayes.git
```

To run the build:

```
stack build
```

To test the code:

```
stack test
```

To open an interactive session:

```
stack ghci
```

[adam-github]: https://github.com/adscib
[adam-web]: https://www.cs.ubc.ca/~ascibior/
[haskell2015-branch]: https://github.com/tweag/monad-bayes/tree/haskell2015
[haskell2015-doi]: https://doi.org/10.1145/2804302.2804317
[icfp2018-doi]: https://doi.org/10.1145/3236778
[models]: https://github.com/tweag/monad-bayes/tree/master/models
[stack-install]: https://docs.haskellstack.org/en/stable/install_and_upgrade/
[thesis-doi]: https://doi.org/10.17863/CAM.42233
[tweagio]: https://tweag.io

# [Monad-Bayes](https://monad-bayes.netlify.app)

<!-- [![Hackage](https://img.shields.io/hackage/v/monad-bayes.svg)](https://hackage.haskell.org/package/monad-bayes)
[![Stackage](http://stackage.org/package/monad-bayes/badge/lts)](http://stackage.org/lts/package/monad-bayes)
[![Hackage Deps](https://img.shields.io/hackage-deps/v/monad-bayes.svg)](http://packdeps.haskellers.com/reverse/monad-bayes)
[![Build status](https://badge.buildkite.com/147af088063e8619fcf52ecf93fa7dd3353a2e8a252ef8e6ad.svg?branch=master)](https://buildkite.com/tweag-1/monad-bayes) -->

[See the website](https://monad-bayes.netlify.app) for an overview of the documentation, library, tutorials, examples (and a link to this very source code). 

<!-- Monad-Bayes is a library for **probabilistic programming in Haskell**. The emphasis is on composition of inference algorithms, and is implemented in terms of monad transformers. -->

<!-- See the [documentation](https://monad-bayes.netlify.app/) for a quick-start user guide and a reference overview of how it all works. -->

Created by [Adam Scibior][adam-web] ([@adscib][adam-github]), documentation, website and various extras by [Reuben][reuben-web], maintained by [Tweag I/O][tweagio].

## Project status

Now that `monad-bayes` has been released on Hackage, we will focus on improving
documentation, adding a variety of applications, improving the API, and making inference more easily customizable.

## Background

The basis for the code in this repository is the ICFP 2018 paper [2]. For the
code associated with the Haskell2015 paper [1], see the [`haskell2015`
tag][haskell2015-tag].

[1] Adam M. Ścibior, Zoubin Ghahramani, and Andrew D. Gordon. 2015. [Practical
probabilistic programming with monads][haskell2015-doi]. In _Proceedings of the
2015 ACM SIGPLAN Symposium on Haskell_ (Haskell ’15), Association for Computing
Machinery, Vancouver, BC, Canada, 165–176.

[2] Adam M. Ścibior, Ohad Kammar, and Zoubin Ghahramani. 2018. [Functional
programming for modular Bayesian inference][icfp2018-doi]. In _Proceedings of
the ACM on Programming Languages_ Volume 2, ICFP (July 2018), 83:1–83:29.

[3] Adam M. Ścibior. 2019. [Formally justified and modular Bayesian inference
for probabilistic programs][thesis-doi]. Thesis. University of Cambridge.

## Hacking

1. Install `stack` by following [these instructions][stack-install].

2. Clone the repository using one of these URLs:
   ```
   git clone git@github.com:tweag/monad-bayes.git
   git clone https://github.com/tweag/monad-bayes.git
   ```

Now you can use `stack build`, `stack test` and `stack ghci`.

To use the notebooks in the `notebooks` directory, you will first need `nix`. Then:

1. Run `nix develop --system x86_64-darwin --extra-experimental-features nix-command --extra-experimental-features flakes`

2. This should open a shell, from which you can run `jupyter-lab` to load the notebooks

[adam-github]: https://github.com/adscib
[adam-web]: https://www.cs.ubc.ca/~ascibior/
[reuben-web]: https://reubencohngordon.com/
[haskell2015-doi]: https://doi.org/10.1145/2804302.2804317
[haskell2015-tag]: https://github.com/tweag/monad-bayes/tree/haskell2015
[icfp2018-doi]: https://doi.org/10.1145/3236778
[models]: https://github.com/tweag/monad-bayes/tree/master/models
[stack-install]: https://docs.haskellstack.org/en/stable/install_and_upgrade/
[thesis-doi]: https://doi.org/10.17863/CAM.42233
[tweagio]: https://tweag.io

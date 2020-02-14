# GHC compatibility and Cabal dependency version bounds

## Overview

`monad-bayes` supports the three most recent [major versions][ghc-major] of
GHC:
- CI builds and tests against **all supported versions**. The CI setup is the
  source of truth for which GHC versions `monad-bayes` supports.
- The local environment (e.g., stack.yaml) sets up **a supported version** of
  GHC.
- The Cabal dependency version bounds for each dependency are as follows:
  The **lower bound** is taken from `cabal gen-bounds` run against the oldest
  supported GHC version. The **upper bound** is taken from `cabal gen-bounds`
  run against the newest supported GHC version.

## What to do when a new major GHC version has been released

A **new major GHC version** has been released. Here's what you need to do:
- **Add the new major GHC version** to the CI build matrix and **drop the
  oldest version** that was previously supported.
- Make sure the the **local environment** (e.g., stack.yaml) still sets up a
  supported version of GHC. If not, update it.
- Update the Cabal **dependency bounds** as described above.

[ghc-major]: https://gitlab.haskell.org/ghc/ghc/wikis/working-conventions/releases#major-releases


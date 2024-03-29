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

## How to release a new version

- Open a separate branch `release` and a merge request. On this branch, do the following:
- Update the file `CHANGELOG.md`, using the diff to the last release.
- Increment the version in `monad-bayes.cabal`. See the [Hackage Package
  Versioning Policy][hackage-pvp].
- Upload the package candidate sources:
  ```console
  $ dir=$(mktemp -d monad-bayes-sdist.XXXXXX)
  $ cabal v2-sdist --builddir="$dir"
  $ cabal upload --user=<hackage user> --password=<hackage password> "$dir/sdist/*.tar.gz"
  $ rm -rf "$dir"
  ```
- Upload the package candidate documentation:
  ```console
  $ dir=$(mktemp -d monad-bayes-docs.XXXXXX)
  $ cabal v2-haddock --builddir="$dir" --haddock-for-hackage --enable-doc
  $ cabal upload --documentation --user=<hackage user> --password=<hackage password> "$dir/*-docs.tar.gz"
  $ rm -rf "$dir"
  ```
- Check the candidate's Hackage page, make sure everything looks as expected.
- When you're ready, and the CI passes for your merge request, repeat the above `cabal upload` commands (for sources and
  documentation), adding `--publish` so the uploads are no longer marked as
  candidates but as proper releases.
- Merge the `release` branch.
- Add a `git` tag in the form `vmajor-major-minor`, e.g. `v1.1.0`, to the commit that was uploaded:
  ```console
  git tag v1.2.3
  git push --tags
  ```

[ghc-major]: https://gitlab.haskell.org/ghc/ghc/wikis/working-conventions/releases#major-releases
[hackage-pvp]: https://pvp.haskell.org/


## Documentation

The docs are built with MkDocs. Serve locally with: `mkdocs serve`. Site is served online with Netlify.

# Benchmarking

## Quick benchmark

* Run `cabal run single -- -m MODEL -a ALG`
  * For `MODEL`, insert e.g. `LR100`
  * For `ALG`, insert e.g. `SMC`
  * See `benchmark/Single.hs` for details

## Extensive benchmark

* Run `cabal bench speed-bench`
* It will run several benchmarks of differing complexity, and try to plot them using Python Pandas
* Look at `samples.pdf`

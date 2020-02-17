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
- When you're ready, repeat the above `cabal upload` commands (for sources and
  documentation), adding `--publish` so the uploads are no longer marked as
  candidates but as proper releases.

[ghc-major]: https://gitlab.haskell.org/ghc/ghc/wikis/working-conventions/releases#major-releases
[hackage-pvp]: https://pvp.haskell.org/

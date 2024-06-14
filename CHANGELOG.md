# 1.3.0.2

- Relaxed some version bounds

# 1.3.0.1

- Support for GHC 9.8

# 1.3.0

- Support for GHC 9.6
- Replaced transformers' `ListT` by
  (https://github.com/tweag/monad-bayes/pull/295)
- Naming fixes for `Sampler` and `SamplerT`

# 1.2.0

- Renamed monad transformers idiomatically
  (https://github.com/tweag/monad-bayes/pull/295)

# 1.1.1

- add fixture tests for benchmark models
- extensive documentation improvements
- add `poissonPdf`
- Fix TUI inference
- Fix flaky test
- Support GHC 9.4

# 1.1.0

- extensive notebook improvements

# 1.0.0 (2022-09-10)

- host website from repo
- host notebooks from repo
- use histogram-fill

# 0.2.0 (2022-07-26)

- rename various functions to match the names of the corresponding types (e.g. `Enumerator` goes with `enumerator`)
- add configs as arguments to inference methods `smc` and `mcmc`
- add rudimentary tests for all inference methods
- put `mcmc` as inference method in new module `Control.Monad.Bayes.Inference.MCMC`
- update history of changelog in line with semantic versioning conventions
- bumped to GHC 9.2.3

# 0.1.5 (2022-07-26)

- Refactor of sampler to be parametric in the choice of a pair of IO monad and RNG

# 0.1.4 (2022-06-15)

Addition of new helper functions, plotting tools, tests, and Integrator monad.

- helpers include: `toEmpirical` (list of samples to empirical distribution) and `toBins` (simple histogramming)
- `Integrator` is an instance of `MonadDistribution` for numerical integration
- `notebooks` now contains working notebook-based tutorials and examples
- new tests, including with conjugate distributions to compare analytic solution against inferred posterior
- `models` directory is cleaned up. New sequential models using `pipes` package to represent monadic streams

# 0.1.3 (2022-06-08)

Clean up of unused functions and broken code

- remove unused functions in `Weighted` and `Population`
- remove broken models in `models`
- explicit imports
- added some global language extensions

# 0.1.2 (2022-06-08)

Add documentation

- docs written in markdown
- docs built by sphinx

# 0.1.1 (2020-04-08)

- New exported function: `Control.Monad.Bayes.Class` now exports `discrete`.

# 0.1.0 (2020-02-17)

Initial release.

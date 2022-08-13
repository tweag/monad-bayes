# 0.2.0 (2021-07-26)

- rename various functions to match the names of the corresponding types (e.g. `Enumerator` goes with `enumerator`)
- add configs as arguments to inference methods `smc` and `mcmc`
- add rudimentary tests for all inference methods
- put `mcmc` as inference method in new module `Control.Monad.Bayes.Inference.MCMC`
- update history of changelog in line with semantic versioning conventions
- bumped to GHC 9.2.3

# 0.1.5 (2021-07-26)

- Refactor of sampler to be parametric in the choice of a pair of IO monad and RNG

# 0.1.4 (2021-06-15)

Addition of new helper functions, plotting tools, tests, and Integrator monad.

- helpers include: `toEmpirical` (list of samples to empirical distribution) and `toBins` (simple histogramming)
- `Integrator` is an instance of `MonadSample` for numerical integration
- `notebooks` now contains working notebook-based tutorials and examples
- new tests, including with conjugate distributions to compare analytic solution against inferred posterior
- `models` directory is cleaned up. New sequential models using `pipes` package to represent monadic streams


# 0.1.3 (2021-06-08)

Clean up of unused functions and broken code

- remove unused functions in `Weighted` and `Population`
- remove broken models in `models`
- explicit imports
- added some global language extensions

# 0.1.2 (2021-06-08)

Add documentation

- docs written in markdown
- docs built by sphinx


# 0.1.1 (2020-04-08)

- New exported function: `Control.Monad.Bayes.Class` now exports `discrete`.

# 0.1.0 (2020-02-17)

Initial release.

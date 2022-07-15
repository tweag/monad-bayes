
# 0.1.1.3 (2021-06-15)

Addition of new helper functions, plotting tools, tests, and Integrator monad.

- helpers include: `toEmpirical` (list of samples to empirical distribution) and `toBins` (simple histogramming)
- `Integrator` is an instance of `MonadSample` for numerical integration
- `notebooks` now contains working notebook-based tutorials and examples
- new tests, including with conjugate distributions to compare analytic solution against inferred posterior
- `models` directory is cleaned up. New sequential models using `pipes` package to represent monadic streams


# 0.1.1.2 (2021-06-08)

Clean up of unused functions and broken code

- remove unused functions in `Weighted` and `Population`
- remove broken models in `models`
- explicit imports
- added some global language extensions

# 0.1.1.1 (2021-06-08)

Add documentation

- docs written in markdown
- docs built by sphinx


# 0.1.1.0 (2020-04-08)

- New exported function: `Control.Monad.Bayes.Class` now exports `discrete`.

# 0.1.0.0 (2020-02-17)

Initial release.



# Getting Started Guide

The Docker image included already contains a build of the library described in
the paper. Thus no setup is necessary beyond starting a Docker container with
the supplied image. If you wish to rebuild the library you can easily do it
using `stack` within the container. Specifically, run the following commands
in the container.

```
cd monad-bayes
stack clean
stack build
stack test
```

You can also rebuild the whole image using the supplied Dockerfile.


# Step-by-Step Instructions

The library described in the paper is included in the Docker image supplied
in the `/home/monad-bayes` folder. Most of the code discussed in the paper is
located in the `src/Control/Monad/Bayes` subfolder.

To run the benchmarks presented in the paper run `stack build monad-bayes:speed-bench`
inside the `/home/monad-bayes` directory in the container. The script thus
executed is located in `benchmark/Speed.hs` and can be modified to change
parameters of the benchmarks or to add different models or probabilistic
programming systems for comparison. To plot the figures from the paper
run `python3 plots.py` from the same directory. Note that if you change the
configuration of the benchmark script you may also need to make corresponding
changes in the plotting script.

Finally, the `profile.sh` script profiles various inference algorithms on
several models as specified in the `benchmark/Single.hs` file. It produces
a collection of `*.prof` files and their postprocessed versions denoted
as `*-small.prof` where cost sites with negligible cost where removed.

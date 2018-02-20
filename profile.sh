#!/bin/sh
# Generates profiling information for selected models and inference algorithms.

stack build --profile
for model in LR HMM LDA
do
  for alg in SMC MH RMSMC
  do
    echo "Profiling $alg on $model"
    stack exec -- example -a $alg -m $model +RTS -p >/dev/null
    mv example.prof $model-$alg.prof
  done
done

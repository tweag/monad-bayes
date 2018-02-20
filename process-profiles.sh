#!/bin/sh
# Processes profiling results to remove clutter.

for model in LR HMM LDA
do
  for alg in SMC MH RMSMC
  do
    file=$model-$alg
    # filter the cost centres with non-zero cost
    sed 's/no location info/no_location_info/' $file.prof  | awk '$8 !~ /0\.0/' >$file-small.prof
  done
done

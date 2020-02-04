#!/usr/bin/env sh
# Generates profiling information for selected models and inference algorithms.

stack build --profile
for model in LR100 HMM100 LDA50
do
  for alg in SMC MH RMSMC
  do
    echo "Profiling $alg on $model"
    stack exec --profile -- example -a $alg -m $model +RTS -p >/dev/null
    file=$model-$alg
    mv example.prof $file.prof
    sed 's/no location info/no_location_info/' $file.prof  | awk '$8 !~ /0\.0/' >$file-small.prof
  done
done

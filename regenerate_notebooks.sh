#!/usr/bin/env sh
# Regenerates the notebook html files in `docs/docs/notebooks/`
#
# Every *.html under docs/docs/notebooks/ that has a source notebook has to be
# produced here, otherwise the pages drift apart: the CI job regenerates and
# then checks `git diff --exit-code`, so whatever this script skips is
# checked-in build output that nothing verifies.

nix --print-build-logs develop .#default-jupyter --command jupyter-nbconvert --to html notebooks/examples/*.ipynb notebooks/tutorials/*.ipynb --output-dir docs/docs/notebooks/

#!/usr/bin/env sh
# Regenerates the notebook html files in `docs/docs/notebooks/`

nix --print-build-logs develop .#default-jupyter --command jupyter-nbconvert --to html notebooks/examples/*.ipynb --output-dir docs/docs/notebooks/

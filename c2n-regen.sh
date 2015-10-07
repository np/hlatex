#!/bin/bash
cabal2nix . > hlatex.nix
# cabal2nix cabal://X-VERSION > nix/X.nix

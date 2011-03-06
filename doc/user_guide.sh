#!/bin/bash
set -xe
cd $(dirname $0)/..
cabal build
mkdir -p out
BASE=generate_hlatex_user_guide
./dist/build/$BASE/$BASE

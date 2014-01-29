#!/bin/bash

cabal sandbox init
for dir in `cat sources.txt`; do
        cabal sandbox add-source $dir
done

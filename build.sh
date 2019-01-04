#!/bin/bash
mkdir -p build
pushd build && \
cmake .. && make -j 4 && \
cd .. && \
time build/cello "$@" && gcc test_mod.o -o out && ./out
popd

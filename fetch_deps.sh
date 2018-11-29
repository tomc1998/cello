#!/bin/bash
mkdir -p deps
pushd deps
git clone https://github.com/mapbox/variant.git
git clone https://github.com/martinmoene/string-view-lite.git
git clone https://github.com/martinmoene/optional-lite.git
popd

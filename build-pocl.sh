#!/bin/bash

[ -d pocl ] && exit 0

git clone --depth=1 https://github.com/pocl/pocl.git

cd pocl

export CXX="g++-4.8"
./autogen.sh
./configure
make



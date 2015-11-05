#!/bin/bash

[ -d pocl ] && exit 0

# temporary fix until merge, this should let the tests pass on CCL
# git clone --depth=1 https://github.com/pocl/pocl.git
git clone --depth=1 https://github.com/guicho271828/pocl.git

cd pocl

export CXX="g++-4.8"
./autogen.sh
./configure
make



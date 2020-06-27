#!/bin/sh -e

git submodule init
git submodule update
git clean -fxd

make -C tests

# verify that forth.bin is stable
cp cvm/forth.bin ref.bin
make -C cvm updatebootstrap
cmp cvm/forth.bin ref.bin
rm ref.bin

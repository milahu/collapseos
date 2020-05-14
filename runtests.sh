#!/bin/sh -e

git submodule init
git submodule update
git clean -fxd

make -C emul
make -C tests

# verify that forth.bin is stable
cp emul/forth.bin ref.bin
make -C emul updatebootstrap
cmp emul/forth.bin ref.bin
rm ref.bin

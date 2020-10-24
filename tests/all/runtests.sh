#!/bin/sh -e

BASE=../..
CVM="${BASE}/cvm/forth"
Z80="${BASE}/emul/forth"
TMP=$(mktemp)

chk() {
    cat ../harness.fs $1 > ${TMP}
    echo "Running test $1 under CVM"
    if ! ${CVM} ${TMP}; then
        exit 1
    fi
    echo "Running test $1 under Z80"
    if ! ${Z80} ${TMP}; then
        exit 1
    fi
}

if [ ! -z $1 ]; then
    chk $1
    exit 0
fi

# those tests run without any builtin
for fn in test_*.fs; do
    chk "${fn}"
done

echo "All tests passed!"
rm ${TMP}

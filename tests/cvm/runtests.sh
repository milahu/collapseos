#!/bin/sh -e

BASE=../..
EXEC="${BASE}/cvm/forth"
TMP=$(mktemp)

chk() {
    echo "Running test $1"
    cat ../harness.fs $1 > ${TMP}
    if ! ${EXEC} ${TMP}; then
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

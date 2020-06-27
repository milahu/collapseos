#!/bin/sh
echo -e "660 LOAD H@ ORG !\n$(cat -)\nORG @ 256 /MOD 2 PC! 2 PC! H@ 256 /MOD 2 PC! 2 PC! " | ./stage

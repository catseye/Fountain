#!/bin/sh -e

./bin/fountain --suppress-newline generate $* > /tmp/fountain.txt
./bin/fountain parse $1 /tmp/fountain.txt

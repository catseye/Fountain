#!/bin/sh -e

./bin/fountain --suppress-newline generate $* > /tmp/fountain.txt
./bin/fountain parse $1 /tmp/fountain.txt $2 $3 $4 $5 $6 $7 $8 $9

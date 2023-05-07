#!/bin/sh -e

./bin/fountain generate $* > /tmp/fountain.txt
./bin/fountain parse $1 /tmp/fountain.txt

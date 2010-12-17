#!/bin/sh

source ../base.sh

runcache

trap killcache INT QUIT TERM EXIT

cacheset "deleter" "foo"

./deleter ${PORT}

if [ $? -ne 0 ]; then
    exit 15;
fi;

cachekeydne "deleter"

echo "ok"

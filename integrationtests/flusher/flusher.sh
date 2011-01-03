#!/bin/sh

source ../base.sh

runcache

trap killcache INT QUIT TERM EXIT

cacheset "flusher" "dummy"

./flusher ${PORT}

if [ $? -ne 0 ]; then
    exit 15;
fi;

cachekeydne "flusher"

echo "ok"

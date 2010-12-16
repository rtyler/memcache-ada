#!/bin/sh

source ../base.sh

runcache

trap killcache INT QUIT TERM EXIT

./stats ${PORT}

if [ $? -ne 0 ]; then
    exit 15;
fi;

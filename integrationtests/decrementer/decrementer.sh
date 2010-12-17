#!/bin/sh

source ../base.sh

runcache

trap killcache INT QUIT TERM EXIT

cacheset "decrementer" "10"

./decrementer ${PORT}

if [ $? -ne 0 ]; then
    exit 15;
fi;

pyexec "import memcache; assert 9 == int(memcache.Client(('127.0.0.1:${PORT}',)).get('decrementer'));";
if [ $? -ne 0 ]; then
    echo ">> Failed to decrement key \"decrementer\"";
    exit 15;
fi;

echo "ok"

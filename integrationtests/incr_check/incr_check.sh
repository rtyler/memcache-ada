#!/bin/sh

source ../base.sh

runcache

trap killcache INT QUIT TERM EXIT

cacheset "incr_check" "1"

./incr_check ${PORT}

if [ $? -ne 0 ]; then
    exit 15;
fi;

pyexec "import memcache; assert 2 == int(memcache.Client(('127.0.0.1:${PORT}',)).get('incr_check'));";
if [ $? -ne 0 ]; then
    echo ">> Failed to increment key \"incrementer\"";
    exit 15;
fi;

echo "ok"

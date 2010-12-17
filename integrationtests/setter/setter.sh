#!/bin/sh

source ../base.sh

runcache

trap killcache INT QUIT TERM EXIT

./setter ${PORT}

if [ $? -ne 0 ]; then
    exit 15;
fi;

pyexec "import memcache; assert 'rawr' == memcache.Client(('127.0.0.1:${PORT}',)).get('setter');";
if [ $? -ne 0 ]; then
    echo ">> Failed to set value for key \"setter\" properly";
    exit 15;
fi;

echo "ok"

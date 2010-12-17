#!/bin/sh

export PORT=11212

function pyexec() {
    PYTHONPATH=../../:$PYTHONPATH python -c "${1}";
}

function killcache() {
    echo ">> Killing cache at pid ${CACHE_PID}"
    kill ${CACHE_PID}
}

function runcache() {
    memcached -p ${PORT} -U 0 -m 1 -v &
    export CACHE_PID=$!
    echo ">> memcached started with pid ${CACHE_PID}";
    echo ">> Waiting for memcached to come online on port ${PORT}";
    sleep 1;
}

function cacheset() {
    pyexec "import memcache; memcache.Client(('127.0.0.1:${PORT}',)).set('${1}', '${2}')";
    if [ $? -ne 0 ]; then
        echo ">> Failed to set key \"${1}\"";
        exit 15;
    fi;
}

function cachekeydne() {
    # Check to verify that a key does not exist
    pyexec "import memcache; assert not memcache.Client(('127.0.0.1:${PORT}',)).get('${1}')"
    if [ $? -ne 0 ]; then
        echo ">> Failed to verify the key \"${1}\" does not exist";
        exit 15;
    fi;
}


#!/bin/sh

echo
echo

PORT=11212

memcached -p ${PORT} -U 0 -m 1 -vv &
CACHE_PID=$!

echo ">> memcached started with pid ${CACHE_PID}"
echo ">> Waiting for memcached to come online on port ${PORT}"
sleep 1 

PYTHONPATH=../../:$PYTHONPATH python -c "import memcache; memcache.Client(('127.0.0.1:${PORT}',)).set('deleter', 'foo')"

./deleter

EXIT_STATUS=$?

if [ "${EXIT_STATUS}" -ne "0" ]; then
    exit ${EXIT_STATUS};
fi;

echo
PYTHONPATH=../../:$PYTHONPATH python -c "import memcache; assert not memcache.Client(('127.0.0.1:${PORT}',)).get('deleter')"
EXIT_STATUS=$?
echo

echo ">> Killing pid ${CACHE_PID}"
kill ${CACHE_PID}

echo ">> Exiting with status ${EXIT_STATUS}"
exit ${EXIT_STATUS}

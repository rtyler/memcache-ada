#!/bin/sh

source ../base.sh

runcache

trap killcache INT QUIT TERM EXIT

DAEMON_VERSION=`memcached -h | head -n 1 | awk '{ print $2 }'`
CLIENT_VERSION=`./versioner ${PORT}`

if [ $? -ne 0 ]; then
    exit 15;
fi;

if [ "${DAEMON_VERSION}" != "${CLIENT_VERSION}" ]; then
    echo "Expected \"${DAEMON_VERSION}\"";
    echo "Mismatch! We got \"${CLIENT_VERSION}\""
    exit 15;
fi;


echo "ok"

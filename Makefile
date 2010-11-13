#
#	A simple Makefile for building and testing memcache-ada

GPRBUILD=gprbuild
TESTRUNNER=testrunner

all:
	mkdir -p build
	$(GPRBUILD) -p memcache.gpr

test: all
	$(GPRBUILD) -p memcachetest.gpr
	./$(TESTRUNNER)

clean:
	rm -rf build
	rm -f $(TESTRUNNER)

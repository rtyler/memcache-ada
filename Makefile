#
#	A simple Makefile for building and testing memcache-ada

GPRBUILD=gprbuild
GPRCLEAN=gprclean
TESTRUNNER=testrunner

lib:
	mkdir -p build
	$(GPRBUILD) -p memcache.gpr

syntax:
	mkdir -p build
	gnatmake -gnatc -gnat05 -P memcache.gpr
	gnatmake -gnatc -gnat05 -P memcachetest.gpr

test: syntax
	$(GPRBUILD) -p memcachetest.gpr
	$(GPRBUILD) -p memcachetestxml.gpr
	./$(TESTRUNNER)

inttest: syntax lib test
	(cd integrationtests && python buildtests.py)
	(cd integrationtests && python runtests.py)

clean:
	$(GPRCLEAN) memcache.gpr
	$(GPRCLEAN) memcachetest.gpr
	(cd integrationtests && python clean.py)
	rm -rf build
	rm -f $(TESTRUNNER)

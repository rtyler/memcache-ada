#
#	A simple Makefile for building and testing memcache-ada

GPRBUILD=gprbuild
GPRCLEAN=gprclean
TESTRUNNER=testrunner

lib: pre
	$(GPRBUILD) -p memcache.gpr

pre:
	mkdir -p build
	mkdir -p obj

syntax: pre
	gnatmake -gnatc -gnat05 -P memcache.gpr
	gnatmake -gnatc -gnat05 -P memcachetest.gpr

test: pre lib syntax
	$(GPRBUILD) -p memcachetest.gpr
	$(GPRBUILD) -p memcachetestxml.gpr
	./$(TESTRUNNER)

inttest: pre syntax lib test
	(cd integrationtests && python buildtests.py)
	(cd integrationtests && python runtests.py)

clean: pre
	$(GPRCLEAN) memcache.gpr
	$(GPRCLEAN) memcachetest.gpr
	(cd integrationtests && python clean.py)
	rm -rf build ob
	rm -f $(TESTRUNNER)
	rm -f *.so*


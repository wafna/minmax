# Handy shortcuts.

# Always out of date.
FORCE: ;

clean: FORCE
	@cabal v2-clean

build: FORCE
	@cabal v2-build

test: FORCE
	@cabal v2-test

test-log: FORCE
	@less ./dist-newstyle/build/x86_64-linux/ghc-8.10.7/MinMax-0.3.0.0/t/test/test/MinMax-0.3.0.0-test.log

trim: FORCE
	@bin/trim-source.sh

sandbox: FORCE
	@cabal v2-run sandbox

docco: FORCE
	@cabal v2-haddock sandbox

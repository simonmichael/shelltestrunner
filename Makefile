# shelltestrunner project makefile

# ensure some locale is set, required by ghc executables
export LANG=en_US.UTF-8

# flag(s) to work around ghc vs. macports issue on mac, if needed
PREFERMACUSRLIBFLAGS=-L/usr/lib

BUILDFLAGS=-idist/build/autogen -threaded -W -fwarn-tabs $(PREFERMACUSRLIBFLAGS) # -Wall
PROGNAME=shelltest
# when running tests, use the latest version to test itself
SHELLTEST=./$(PROGNAME) --with ./$(PROGNAME) -j8
TESTFILES=tests.format*/*.test
HSFILES=*.hs Utils/*.hs

######################################################################
# BUILD

build:
	stack build

shelltest.ghc-%: shelltest.hs
	ghc-$* --make $(BUILDFLAGS) $(PROGNAME).hs -o $@ -outputdir .ghc-$*

shelltest.ghcall: \
	shelltest.ghc-7.4.1 \
	shelltest.ghc-7.2.2 \
	shelltest.ghc-7.0.4 \
	shelltest.ghc-6.12.3 \

test: testunix

# run cross-platform and unix-specific tests
testunix: build
	$(SHELLTEST) tests.format1 tests.format1.unix tests.format2 tests.format2.unix tests.format2b tests.format2b.unix

# run cross-platform and windows-specific tests
# (though if you are able to run make on windows, you may be able to/have to use testunix)
testwindows:
	$(SHELLTEST) tests.format1 tests.format1.windows tests.format2 tests.format2.windows tests.format2b tests.format2b.windows

# run tests with a specific GHC version
test-ghc-%: shelltest.ghcall
	@echo; echo testing shelltest built with ghc-$*
	@(./shelltest.ghc-$* --with ./shelltest.ghc-$* tests* --exclude windows -j8 --hide-successes \
	&& echo $@ PASSED) || echo $@ FAILED

test-ghcall: \
	test-ghc-7.4.1 \
	test-ghc-7.2.2 \
	test-ghc-7.0.4 \
	test-ghc-6.12.3 \

ghci:
	ghci -idist/build/autogen shelltest.hs -fdefer-type-errors

######################################################################
# DOC

# called on each darcs commit
commithook: site

docs: site haddock

# build website using my standard site build script
# (twice, to help it link index.html)
.PHONY: site
site:
	hakyll build
	hakyll build

cleansite:
	hakyll clean

# re-copy/render files on change, also serve them on port 8000
# preview allows remote clients unlike the newer watch command
previewsite:
	hakyll preview

#VIEWHTML=firefox
VIEWHTML=open

viewsite: site
	$(VIEWHTML) _site/index.html

# build haddock docs
haddock:
	cabal configure && cabal haddock --executables

viewhaddock: docs
	$(VIEWHTML) dist/doc/html/shelltestrunner/$(EXE)/index.html

######################################################################
# MISC

showloc:
	@echo Current lines of code including tests:
	@sloccount *.hs | grep haskell:
	@echo

tag: $(HSFILES) $(TESTFILES) *.md Makefile
	hasktags -e -o TAGS $^
	hasktags -c -o ctags $^

clean:
	rm -f `find . -name "*.o" -o -name "*.hi" -o -name "*~" -o -name "darcs-amend-record*" -o -name "*-darcs-backup*"`

Clean: clean
	rm -f TAGS _cache #_site $(EXE)

# shelltestrunner project makefile

# ghc 6.12 executables need a locale
export LANG=en_US.UTF-8

# flag(s) to work around ghc vs. macports issue on mac, if needed
PREFERMACUSRLIBFLAGS=-L/usr/lib

BUILDFLAGS=-threaded -W -fwarn-tabs -Werror $(PREFERMACUSRLIBFLAGS)
PROGNAME=shelltest
# when running tests, use the latest version to test itself
SHELLTEST=./$(PROGNAME) --with ./$(PROGNAME)

######################################################################
# BUILD

build:
	ghc --make $(BUILDFLAGS) $(PROGNAME).hs

shelltest.ghc-%: shelltest.hs
	ghc-$* --make $(BUILDFLAGS) $(PROGNAME).hs -o $@ -outputdir .ghc-$*

shelltest.ghcall: \
	shelltest.ghc-7.4.1 \
	shelltest.ghc-7.2.2 \
	shelltest.ghc-7.0.4 \
	shelltest.ghc-6.12.3 \

AUTOBUILDCMDARGS=tests
autobuild auto:
	sp --no-exts --no-default-map -o $(PROGNAME) ghc --make $(BUILDFLAGS) $(PROGNAME).hs --run $(AUTOBUILDCMDARGS)

# on unix, run all except windows tests
testunix test: build
	$(SHELLTEST) tests --exclude windows -- -j8

# on windows, run all except unix tests
# (though if you are able to run make on windows, you may be able to/have to use testunix)
testwindows:
	$(SHELLTEST) tests --exclude unix -- -j8

# run tests with a specific GHC version
test-ghc-%: shelltest.ghcall
	@echo; echo testing shelltest built with ghc-$*
	@(./shelltest.ghc-$* --with ./shelltest.ghc-$* tests --exclude windows -- -j8 --hide-successes \
	&& echo $@ PASSED) || echo $@ FAILED

test-ghcall: \
	test-ghc-7.4.1 \
	test-ghc-7.2.2 \
	test-ghc-7.0.4 \
	test-ghc-6.12.3 \

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
# RELEASE

TARBALL:=$(shell cabal sdist | tail -1 | cut -d' ' -f4)
VERSION:=$(shell echo $(TARBALL) | cut -d- -f2 | cut -d. -f1-2) # XXX or -f1-3

showversion:
	@echo $(VERSION)

tagrepo:
	@(darcs show tags | grep -q "^$(VERSION)$$") && echo tag $(VERSION) already present || darcs tag $(VERSION)

upload:
	(cabal upload $(TARBALL) --check | grep '^Ok$$') \
		&& cabal upload $(TARBALL) \
		|| (cabal upload $(TARBALL) --check -v3; false)

push:
	darcs push -a joyful.com:/repos/shelltestrunner

release: test tagrepo upload #push

# show project stats useful for release notes
releasestats stats: \
	showreleasedays \
	showunreleasedchangecount \
	showloc \
	showreleaseauthors \
	showunreleasedcodechanges \
	showunpushedchanges
#	showtestcount \
#	showunittestcoverage \

showreleasedays:
	@echo Days since last release:
	@../hledger/tools/dayssincerelease.hs | head -1 | cut -d' ' -f-1
	@echo

showunreleasedchangecount:
	@echo Commits since last release:
	@darcs changes --from-tag . --count
	@echo

showreleaseauthors:
	@echo Patch authors since last release:
	@darcs changes --from-tag . |grep '^\w' |cut -c 31- |sort |uniq
	@echo

showloc:
	@echo Current lines of code including tests:
	@sloccount `find . -name '*hs'` | grep haskell:
	@echo

# showtestcount:
# 	@echo "Unit tests:"
# 	@hledger test 2>&1 | cut -d' ' -f2
# 	@echo "Functional tests:"
# 	@make --no-print functest | egrep '^ Total' | awk '{print $$2}'
# 	@echo

# showunittestcoverage:
# 	@echo Unit test coverage:
# 	@make --no-print quickcoverage | grep 'expressions'
# 	@echo

# showerrors:
# 	@echo Known errors:
# 	@awk '/^** errors/, /^** / && !/^** errors/' NOTES | grep '^\*\*\* ' | tail +1
# 	@echo

showunpushedchanges unpushed:
	@echo "Changes not yet pushed upstream (to `darcs show repo | grep 'Default Remote' | cut -c 17-`):"
	@-darcs push --dry-run | grep '*' | tac
	@echo

showunreleasedcodechanges unreleased:
	@echo "code changes since last release:"
	@darcs changes --from-tag . --matches "not (name docs: or name doc: or name site: or name tools:)" | egrep '^  \* '
	@echo

showcodechanges:
	@echo "code changes:"
	@darcs changes --matches "not (name docs: or name site: or name tools:)" | egrep '^ +(\*|tagged)'
	@echo

######################################################################
# MISC

# tag: emacstags
# emacstags:
# 	rm -f TAGS; hasktags -e *hs *.cabal tests/*.test

TAG=hasktags -e

tag: TAGS

TAGS: *.hs *.md Makefile
	$(TAG) *.hs *.md tests/*.test Makefile

clean:
	rm -f `find . -name "*.o" -o -name "*.hi" -o -name "*~" -o -name "darcs-amend-record*" -o -name "*-darcs-backup*"`

Clean: clean
	rm -f TAGS _cache #_site $(EXE)

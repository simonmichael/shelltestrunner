# shelltestrunner project makefile

# ensure some locale is set, required by ghc executables
export LANG=en_US.UTF-8

# the shelltest executable built with default stack resolver/ghc version
DEFAULTEXE=$(shell stack path --local-install-root)/bin/shelltest

# default flags when running shelltest
SHELLTEST=$(DEFAULTEXE) --exclude /_ -j16 --hide-successes

# files to tag
TESTFILES=tests/format*/*.test
HSFILES=src/*.hs src/Utils/*.hs

# Several stack resolvers/ghc versions to try building/testing with.
# Not all of these work on all platforms, eg ghc 7.8 on OSX Sierra.
# | lts    | ghc    |
# | 10     | 8.2.2  |
# | 9,8    | 8.0.2  |
# | 7      | 8.0.1  |
# | 6,5,4  | 7.10.3 |
# | 3      | 7.10.2 |
# | 2,1    | 7.8.4  |
RESOLVERS=\
	nightly \
	lts-10 \
	lts-9 \
	lts-6 \

default: build

# commithook: site

# BUILD

# build with default resolver/ghc version
build:
	stack build

install:
	stack install

build-all: build-with-resolvers

# build, run any unit tests/benchmarks, and check haddock with several ghc versions
build-with-resolvers: $(foreach r,$(RESOLVERS),build-with-resolver-$r)

build-with-resolver-%:
	stack --resolver=$* build --test --bench --haddock --no-haddock-deps
#	stack --resolver=$* clean  # needed for more thorough test ?

# REPL

ghci:
	stack ghci # -fdefer-type-errors

# TEST

test: testunix testexamples testbash

# run cross-platform and unix-specific shell tests with default shelltest build
testunix: build
	$(SHELLTEST) tests -x /bash -x /examples -x .windows -w $(DEFAULTEXE)

# run cross-platform and windows-specific shell tests
# (though if you are running make on windows, you may be able to, or have to, use testunix)
testwindows:
	$(SHELLTEST) tests -x /bash -x /examples -x .unix -w $(DEFAULTEXE)

testexamples: build
	$(SHELLTEST) tests/examples

testbash: build
	! $(SHELLTEST) tests/bash
	$(SHELLTEST) tests/bash --shell /bin/bash

# run shell tests with several ghc versions
# test-with-resolvers: build-with-resolvers $(foreach r,$(RESOLVERS),test-with-resolver-$r)

# test-with-resolver-%:
# 	echo $(stack --resolver=$* path --local-install-root)/bin/shelltest tests

# 	@echo; echo testing shelltest built with ghc-$*
# 	@(./shelltest.ghc-$* --with ./shelltest.ghc-$* tests* --exclude windows -j8 --hide-successes \
# 	&& echo $@ PASSED) || echo $@ FAILED

# DOCS

PANDOC=pandoc -f gfm -s -M pagetitle:'shelltestrunner - Easy, repeatable testing of CLI programs/commands'

# convert any markdown files to html
html: $(patsubst %.md,%.html,$(wildcard *.md)) Makefile

# generate html from a md file
%.html: %.md #index.tmpl
	$(PANDOC) $< -o $@ #--template index.tmpl 

# regenerate html files when the corresponding markdown file changes
liverender:
	ls *.md | entr make html

# reload the page in a browser viewing http://localhost:10000/README.html (eg) when the file changes
livereload:
	livereloadx -p 10000 --static .

LASTTAG=$(shell git describe --tags --abbrev=0)

changes-show: $(call def-help,changes-show, show commits affecting the current directory excluding any hledger package subdirs from the last tag as org nodes newest first )
	@make changes-show-from-$(LASTTAG)

changes-show-from-%: #$(call def-help,changes-show-from-REV, show commits affecting the current directory excluding any hledger package subdirs from this git revision onward as org nodes newest first )
	@git log --abbrev-commit --pretty=format:'ORGNODE %s (%an)%n%b%h' $*.. -- . ':!hledger' ':!hledger-*' \
		| sed -e 's/^\*/-/' -e 's/^ORGNODE/*/' \
		| sed -e 's/ (Simon Michael)//'

# docs: site #haddock

# # build haddock docs
# haddock:
# 	cabal configure && cabal haddock --executables

# viewhaddock: docs
# 	$(VIEWHTML) dist/doc/html/shelltestrunner/$(EXE)/index.html

#VIEWHTML=firefox
#VIEWHTML=open

# site-view: site
# 	$(VIEWHTML) _site/index.html

# MISC

loc:
	@echo Current lines of code including tests:
	@sloccount src | grep haskell:

tag: $(HSFILES) #$(TESTFILES) *.md Makefile
	hasktags -e -o TAGS $^
	hasktags -c -o ctags $^

# clean:
# 	rm -f `find . -name "*.o" -o -name "*.hi" -o -name "*~" -o -name "darcs-amend-record*" -o -name "*-darcs-backup*"`

# Clean: clean
# 	rm -f TAGS _cache #_site $(EXE)

# shelltestrunner project makefile

# ensure a utf8-aware locale is set, required by ghc executables
export LANG = en_US.UTF-8

# which stack.yaml file (& ghc version) to use, can be overridden by STACK env var
STACKYAML ?= stack.yaml

# used below and also in tests which run stack
STACKYAMLOPT = --stack-yaml=$(STACKYAML)

# the current base stack command
STACK = stack $(STACKYAMLOPT)

# the shelltest executable built with current stack
SHELLTESTEXE = $(shell $(STACK) path --local-install-root)/bin/shelltest

# the base shelltest command with common options,
# and the STACKYAMLOPT env var which helps any tests
# which run stack themselves (eg large-output.test)
SHELLTEST = STACKYAMLOPT=$(STACKYAMLOPT) $(SHELLTESTEXE) --exclude /_ -j16 --hide-successes

# standard targets

default: build

build:
	$(STACK) build

install:
	$(STACK) install

ghci:
	$(STACK) ghci

ghcid:
	$(STACK) exec -- ghcid

test: testcross testunix testbash testexamples

# tests

# run cross-platform shell tests
testcross: build
	@echo; echo "cross-platform tests should succeed:"
	$(SHELLTEST) -w $(SHELLTESTEXE) tests -x /bash -x /examples -x .windows -x .unix -w $(SHELLTESTEXE)

# run unix-specific shell tests
testunix: build
	@echo; echo "on unix, unix tests should succeed:"
	$(SHELLTEST) -w $(SHELLTESTEXE) tests/*.unix

# run windows-specific shell tests
# (though if you are using make on windows, you may be able to, or may have to, use testunix)
testwindows: build
	@echo; echo "on windows, windows tests should succeed:"
	@$(SHELLTEST) -w $(SHELLTESTEXE) tests/*.windows

# run bash-specific shell tests
testbash: build
	@echo; echo "when using bash, bash tests should succeed:"
	@$(SHELLTEST) tests/bash --shell /bin/bash

# run tests of the README examples
testexamples: build
	@echo; echo "README examples should succeed:"
	@$(SHELLTEST) tests/examples

# misc

LASTTAG=$(shell git describe --tags --abbrev=0)

changes-show: $(call def-help,changes-show, show commits affecting the current directory excluding any hledger package subdirs from the last tag as org nodes newest first )
	@make changes-show-from-$(LASTTAG)

changes-show-from-%: #$(call def-help,changes-show-from-REV, show commits affecting the current directory excluding any hledger package subdirs from this git revision onward as org nodes newest first )
	@git log --abbrev-commit --pretty=format:'ORGNODE %s (%an)%n%b%h' $*.. -- . ':!hledger' ':!hledger-*' \
		| sed -e 's/^\*/-/' -e 's/^ORGNODE/*/' \
		| sed -e 's/ (Simon Michael)//'

loc:
	@echo Current lines of code including tests:
	@sloccount src | grep haskell:

# files to tag
HSFILES=src/*.hs src/Utils/*.hs
TESTFILES=tests/format*/*.test

tag: $(HSFILES) #$(TESTFILES) *.md Makefile
	hasktags -e -o TAGS $^
	hasktags -c -o ctags $^

# clean:
# 	rm -f `find . -name "*.o" -o -name "*.hi" -o -name "*~" -o -name "darcs-amend-record*" -o -name "*-darcs-backup*"`

# Clean: clean
# 	rm -f TAGS _cache #_site $(EXE)

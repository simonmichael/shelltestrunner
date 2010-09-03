# shelltestrunner project makefile

# ghc 6.12 executables need a locale
export LANG=en_US.UTF-8

# flag(s) to work around ghc vs. macports issue on mac, if needed
PREFERMACUSRLIBFLAGS=-L/usr/lib

BUILDFLAGS=-threaded -W -fwarn-tabs -Werror $(PREFERMACUSRLIBFLAGS)
EXE=shelltest

######################################################################
# BUILD

build:
	ghc --make $(BUILDFLAGS) $(EXE).hs

AUTOBUILDCMDARGS=tests
autobuild auto:
	sp --no-exts --no-default-map -o $(EXE) ghc --make $(BUILDFLAGS) $(EXE).hs --run $(AUTOBUILDCMDARGS)

test: build
	./$(EXE) tests -j8

######################################################################
# DOC

# build project website
# Requires hakyll (cabal install hakyll)
.PHONY: site
site: site/hakyll site/_site/index.html
	cd site; ./hakyll build

site/_site/index.html:
	cd site/_site; ln -sf README.html index.html

site/hakyll: site/hakyll.hs
	cd site; ghc --make hakyll.hs $(PREFERMACUSRLIBFLAGS)

cleansite: site/hakyll
	cd site; ./hakyll clean

previewsite: site/hakyll site/_site/index.html
	cd site; ./hakyll preview

autobuildsite: site/_site/index.html
	cd site; sp --no-exts --no-default-map -o hakyll ghc --make hakyll.hs $(PREFERMACUSRLIBFLAGS) --run preview

viewsite: site site/_site/index.html
	$(VIEWHTML) site/_site/index.html

docs haddock:
	cabal configure && cabal haddock --executables

######################################################################
# RELEASE

TARBALL:=$(shell cabal sdist | tail -1 | cut -d' ' -f4)
VERSION:=$(shell echo $(TARBALL) | cut -d- -f2 | cut -d. -f1-2)

# called on each darcs commit, if configured in this repo's posthook
commithook: site

showversion:
	@echo $(VERSION)

tagrepo:
	@(darcs show tags | grep -q "^$(VERSION)$$") && echo tag $(VERSION) already present || darcs tag $(VERSION)

push:
	darcs push -a joyful.com:/repos/shelltestrunner

release: test tagrepo push
	(cabal upload $(TARBALL) --check | grep '^Ok$$') \
		&& cabal upload $(TARBALL) \
		|| (cabal upload $(TARBALL) --check -v3; false)

######################################################################
# MISC

tag: emacstags

emacstags:
	rm -f TAGS; hasktags -e *hs *.cabal tests/*.test

clean:
	rm -f `find . -name "*.o" -o -name "*.hi" -o -name "*~" -o -name "darcs-amend-record*" -o -name "*-darcs-backup*"`

Clean: clean
	rm -f TAGS $(EXE)

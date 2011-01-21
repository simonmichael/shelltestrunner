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

# called on each darcs commit
commithook: site

docs: site haddock

#VIEWHTML=google-chrome
VIEWHTML=open

viewsite: site
	$(VIEWHTML) index.html

viewhaddock: docs
	$(VIEWHTML) dist/doc/html/shelltestrunner/$(EXE)/index.html 

# build website
site: sitebuild

sitebuild: hakyll
	./hakyll build

siteclean: hakyll
	./hakyll clean

sitepreview: hakyll
	./hakyll preview 8002

hakyll: hakyll.hs
	ghc --make -Wall hakyll.hs

# build haddock docs
haddock:
	cabal configure && cabal haddock --executables

######################################################################
# RELEASE

TARBALL:=$(shell cabal sdist | tail -1 | cut -d' ' -f4)
VERSION:=$(shell echo $(TARBALL) | cut -d- -f2 | cut -d. -f1-2)

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

# tag: emacstags
# emacstags:
# 	rm -f TAGS; hasktags -e *hs *.cabal tests/*.test

tag: TAGS

TAGS: *.el *.hs *.markdown Makefile
	etags *.el *.hs *.markdown Makefile

clean:
	rm -f `find . -name "*.o" -o -name "*.hi" -o -name "*~" -o -name "darcs-amend-record*" -o -name "*-darcs-backup*"`

Clean: clean
	rm -f TAGS _cache #_site $(EXE)

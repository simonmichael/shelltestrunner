# shelltestrunner project makefile

build:
	ghc --make -threaded -Wall shelltest.hs

test: build
	./shelltest -j8

TARBALL:=$(shell cabal sdist | tail -1 | cut -d' ' -f4)
VERSION:=$(shell echo $(TARBALL) | cut -d- -f2 | cut -d. -f1-3)

showversion:
	@echo $(VERSION)

tagrepo:
	@(darcs show tags | grep -q "^$(VERSION)$$") && echo tag $(VERSION) already present || darcs tag $(VERSION)

push:
	darcs push -a joyful.com:/repos/shelltestrunner

release: test tagrepo push
	cabal sdist
	(cabal upload $(TARBALL) --check | grep '^Ok$$') \
		&& cabal upload $(TARBALL) \
		|| (cabal upload $(TARBALL) --check -v3; false)

docs haddock:
	cabal configure && cabal haddock --executables

# site: push docs
# 	rsync -avz dist/doc/html/shelltestrunner/shelltestrunner/ joyful.com:/repos/shelltestrunner/html/

tag: emacstags

emacstags:
	rm -f TAGS; hasktags -e *hs *.cabal tests/*.test

clean:
	rm -f `find . -name "*.o" -o -name "*.hi" -o -name "*~" -o -name "darcs-amend-record*" -o -name "*-darcs-backup*"`

Clean: clean
	rm -f TAGS shelltestrunner

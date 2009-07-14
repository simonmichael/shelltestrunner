# shelltestrunner project makefile

build:
	ghc --make -threaded -Wall shelltestrunner.hs

test:
	shelltestrunner shelltestrunner.hs *.test -- -j8

TARBALL:=$(shell cabal sdist | tail -1 | cut -d' ' -f4)
VERSION:=$(shell echo $(TARBALL) | cut -d- -f2 | cut -d. -f1-2)

tagrepo:
	@(darcs show tags | grep -q "^$(VERSION)$$") && echo tag $(VERSION) already present || darcs tag $(VERSION)

upload: tagrepo
	cabal sdist
	(cabal upload $(TARBALL) --check | grep '^OK$$') \
		&& cabal upload $(TARBALL) \
		|| (cabal upload $(TARBALL) --check -v3; false)
	darcs push -a joyful.com:/repos/shelltestrunner


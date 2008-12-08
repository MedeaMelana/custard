default:
	runghc Main 5678

mtag = $(shell svn info | grep URL | sed 's/^.*Custard-//g')
tag  = $(subst URL: svn://localhost/Custard/trunk,99.99,$(mtag))

cabal-config: clean
	sed "s/@tag/$(tag)/g" < Custard.cabal-template > Custard.cabal
	runghc Setup configure

package: cabal-config
	runghc Setup sdist

docs: cabal-config
	runghc Setup haddock

clean:
	find . -name '*.hi' -or -name '*.o' | xargs -n 1 rm
	rm -rf dist build Custard.cabal

PACKAGE=$(APP)

build-haskell:
	cd haskell; LANG=C.UTF-8; stack build

test-haskell:
	cd haskell; LANG=C.UTF-8; stack test

hlint:
	hlint .

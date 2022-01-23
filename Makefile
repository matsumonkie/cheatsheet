PACKAGE=$(APP)

watch-haskell:
	cd haskell; LANG=C.UTF-8; ghcid --command 'stack ghci cheatsheet --test --main-is cheatsheet:test:spec' --warnings

build-haskell:
	cd haskell; LANG=C.UTF-8; stack build

test-haskell:
	cd haskell; LANG=C.UTF-8; stack test

hlint:
	hlint .

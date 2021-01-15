build:
	hpack .
	cabal -j4 build

test:
	hpack .
	cabal test

ghcid:
	hpack .
	ghcid -c 'cabal repl'

ghcid-test:
	hpack .
	ghcid -c 'cabal repl backend:tests'

ghcid-with-tests:
	hpack .
	ghcid -c 'cabal repl backend:tests' --test 'Main.main'

format:
	find test/ src/ app/ -name "*.hs" -exec ormolu -i {} \;

hoogle:
	hoogle server

repl:
	hpack .
	cabal repl

repl-test:
	hpack .
	cabal repl backend:tests

hlint:
	hpack .
	hlint .

.PHONY: build test ghcid ghcid-test format hoogle repl repl-test ghcid-with-tests hlint

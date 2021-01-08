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

format:
	find test/ src/ app/ -name "*.hs" -exec ormolu -i {} \;

hoogle:
	hoogle server

.PHONY: build test ghcid ghcid-test format hoogle

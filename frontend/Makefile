build:
	spago build

ghcid:
	spago build --watch

codegen:
	spago build --purs-args '--codegen corefn'

bundle:
	parcel build assets/index.html --public-url '.' --no-source-maps

serve:
	make codegen
	make bundle
	http-server dist

.PHONY: build codegen bundle serve

build:
	spago build

watch:
	spago build --watch

codegen:
	spago build --purs-args '--codegen corefn'
	spago bundle-app --main Main --to assets/index.js

bundle:
	parcel build assets/index.html --public-url '.' --no-source-maps

serve:
	rm -r dist/ assets/index.js
	make codegen
	make bundle
	http-server dist

format:
	find src/ -name "*.purs" -exec purty --write {} \;

.PHONY: build watch codegen bundle serve format


.PHONY: cabal-build
cabal-build:
	cabal new-build

.PHONY: stack-build
stack-build:
	stack build

.PHONY: weeder
weeder:
	weeder . --build

.PHONY: format
format:
	find . -name '*.hs' -exec echo "Formatting '{}'" \; -exec ormolu --mode=inplace '{}' \;

.PHONY: watch
watch:
	ghcid

.PHONY: install-brew-deps
install-brew-deps:
	brew install cairo libffi
	echo "WARNING"
	echo "======="
	echo "If the build process complains about libffi and pkg-config, try"
	echo "export PKG_CONFIG_PATH=/usr/local/opt/libffi/lib/pkgconfig"

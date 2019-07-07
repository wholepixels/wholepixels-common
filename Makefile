
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
	find src -name '*.hs' -exec echo "Formatting '{}'" \; -exec ormolu --config=ormolu.yaml --mode=inplace '{}' \;

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

.PHONY: release
release:
	stack sdist
	stack upload .

.PHONY: vim
vim:
	echo ":e wholepixels.cabal\n:vsplit\n:e src/WholePixels.hs\n:vsplit\n:term ghcid\n:split\n:term" | nvim -s -

build:
	stack build njtransit-alerts
	ls -1 *.hs | xargs hlint

build-prof:
	stack build --profile --ghc-options="-rtsopts" njtransit-alerts

install:
	stack install njtransit-alerts

clean:
	stack clean

hoogle:
	stack hoogle --server

.PHONY: \
  build \
  build-prof \
  clean


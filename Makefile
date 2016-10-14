build:
	stack build njtransit-alerts

build-prof:
	stack build --profile --ghc-options="-rtsopts" njtransit-alerts 

install:
	stack install njtransit-alerts

clean:
	stack clean

tags:
	hasktags-generate .

sources:
	stack-unpack-dependencies

.PHONY: build build-prof clean tags sources


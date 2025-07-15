HTML_FILE=$(shell pwd)/_build/default/bin/index.html

.PHONY: all fmt build url clean deps test

all: fmt build

fmt:
	refmt */*.re --in-place
	refmt */*.rei --in-place

build:
	dune build bin/main.bc.js
	dune build bin/index.html

url:
	@echo "file://$(HTML_FILE)"

clean:
	dune clean

deps:
	opam install dune reason incr_dom ocaml-lsp-server ppx_deriving

test:
	dune test

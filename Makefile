all: fmt build open

fmt:
	refmt */*.re --in-place
	refmt */*.rei --in-place

build:
	dune build bin/{main.bc.js,index.html}

open:
	open _build/default/bin/index.html

clean:
	dune clean

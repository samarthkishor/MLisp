default:
	opam update
	opam install . --deps-only
	dune build

build:
	dune build

install:
	opam update
	opam install --yes . --deps-only

test:
	dune runtest 

clean:
	dune clean

fmt:
	dune build @fmt --auto-promote

coverage:
	dune clean
	BISECT_ENABLE=yes dune build || true  # ignore dune build error
	dune runtest
	bisect-ppx-report html
	bisect-ppx-report summary

repl:
	dune utop

.PHONY: build exec clean

default:
	dune build

build:
	dune build

exec:
	dune build
	dune exec ./main.exe

clean:
	dune clean

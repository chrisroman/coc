TESTS := tests/parse/*.coc \
	tests/typecheck/*.coc

default:
	dune build main.exe

.PHONY: build
build:
	dune build main.exe

.PHONY: test
test:
	turnt $(TESTS)

.PHONY: save
save:
	turnt --save $(TESTS)

.PHONY: clean
clean:
	dune clean

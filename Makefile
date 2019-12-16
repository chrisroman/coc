TESTS := tests/parse/*.coc \
	tests/typecheck/*.coc

default:
	dune build main.exe

.PHONY: build
build:
	dune build main.exe

.PHONY: test
test:
	turnt -v $(TESTS)

.PHONY: debug
debug:
	turnt -v $(TESTS) -a "--debug"

.PHONY: save
save:
	turnt --save $(TESTS)

.PHONY: repl
repl:
	_build/default/main.exe --repl

.PHONY: clean
clean:
	dune clean

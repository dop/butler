all: main.exe

main.exe: _build/default/bin/main.exe
	cp _build/default/bin/main.exe .

_build/default/bin/main.exe: bin/*.ml
	jbuilder build --debug-backtraces bin/main.exe

check: _build/default/test/tests.bc
	./_build/default/test/tests.bc -v --color=always

_build/default/test/tests.bc: test/*.ml
	jbuilder build --debug-backtraces test/tests.bc

.PHONY: clean

clean:
	jbuilder clean

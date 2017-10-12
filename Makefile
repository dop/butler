BUILD=jbuilder build --dev --debug-backtraces

all: main.exe

main.exe: _build/default/bin/main.exe
	cp _build/default/bin/main.exe .

_build/default/bin/main.exe: bin/*.ml lib/*.ml lib/*.mli bin/jbuild lib/jbuild
	$(BUILD) bin/main.exe

check: _build/default/test/tests.bc
	./_build/default/test/tests.bc -v --color=always

_build/default/test/tests.bc: main.exe test/*.ml test/jbuild
	$(BUILD) test/tests.bc

.PHONY: clean

clean:
	jbuilder clean

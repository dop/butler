JB=jbuilder build --dev --debug-backtraces

.PHONY: clean check all

all:
	$(JB) bin/main.exe
	cp _build/default/bin/main.exe .

check:
	$(JB) test/tests.bc
	_build/default/test/tests.bc --color=always -v

clean:
	jbuilder clean

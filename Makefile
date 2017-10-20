JB=jbuilder build --dev --debug-backtraces

all: main.exe

install: main.exe
	cp main.exe ~/bin/butler

.PHONY: clean check main.exe

main.exe:
	$(JB) bin/main.exe
	cp _build/default/bin/main.exe .

check:
	$(JB) test/tests.bc
	_build/default/test/tests.bc --color=always

clean:
	jbuilder clean

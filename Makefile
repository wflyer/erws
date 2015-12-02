NAME=erws

all: deps compile release
	./rebar compile

deps:
	./rebar get-deps

compile:
	./rebar compile

release:
	cd rel && ../rebar generate && cd -

node:
	(cd rel && ../rebar create-node nodeid=${NAME} && cd -)

clean:
	./rebar clean
	rm -rf rel/${NAME}

run:
	rel/${NAME}/bin/${NAME} start

stop:
	rel/${NAME}/bin/${NAME} stop

runconsole:
	rel/${NAME}/bin/${NAME} console

alldev: clean all runconsole

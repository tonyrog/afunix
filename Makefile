.PHONY: all clean clean-all compile xref ci test

REBAR3 ?= rebar3

all: compile xref

ci: compile xref test

compile:
	$(REBAR3) compile

clean:
	$(REBAR3) clean

clean-all: clean
	rm -rf _build

xref:
	$(REBAR3) xref

test:
	$(REBAR3) ct

#@BEGIN-C_SRC-DEFAULT-RULES@
ERL=$(shell readlink -f `which erl`)
ERL_TOP=$(ERL:%/bin/erl=%)
OS=$(shell uname -s)
CFLAGS=-MMD -MP -MF .$<.d -I$(ERL_TOP)/usr/include
#@END-C_SRC-DEFAULT-RULES@

CFLAGS+=-std=c99 -Wall -O3
LDFLAGS+=-shared

NIF = afunix_api_SUITE.so

all: $(NIF)

afunix_api_SUITE.so: afunix_api_SUITE.o
	$(CC) $(LDFLAGS) afunix_api_SUITE.o -o $@

%.o:	%.c
	$(CC) $(CFLAGS) -c -fPIC $<

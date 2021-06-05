#@BEGIN-C_SRC-DEFAULT-RULES@
ERL=$(shell readlink -f `which erl`)
ERL_TOP=$(ERL:%/bin/erl=%)
OS=$(shell uname -s)
CFLAGS=-MMD -MP -MF .$<.d -I$(ERL_TOP)/usr/include
#@END-C_SRC-DEFAULT-RULES@

APP=afunix
CFLAGS+=-std=c99 -Wall -O3
LDFLAGS+=-shared

OBJS = afunix_drv.o packet_parser.o
LIBS = 

all: ../priv/$(APP)_drv.so

../priv/$(APP)_drv.so: $(OBJS)
	$(CC) $(LDFLAGS) $(OBJS) $(LIBS) -o ../priv/$(APP)_drv.so

clean:
	rm -f $(OBJS) ../priv/$(APP)_drv.so .*.d

%.o:	%.c
	$(CC) $(CFLAGS) -c -fPIC $<

.%.d:	;

-include .*.d

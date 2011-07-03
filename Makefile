SRCS = bitarray.c ecpp.c main.c sieve.c

HEADERS = bitarray.h ecpp.h sieve.h

CC = gcc
CFLAGS = -W -Wall -pedantic -ansi -g -DDEBUG
LDFLAGS = -lgmp

ecpp: $(subst .c,.o,$(SRCS))

.PHONY: clean
clean:
	$(RM) ecpp *.o


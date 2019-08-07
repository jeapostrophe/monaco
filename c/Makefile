all: ttt.test

CFLAGS := -Wall -Werror -Wpedantic -O3 -march=native -Wno-unused-variable -Wno-unused-function --std=c17 

%.test: %.bin
	./$<

%.bin: %.o
	cc $(CFLAGS) $^ -o $@

%.o: %.c monaco.h monaco.c
	cc $(CFLAGS) $< -c -o $@

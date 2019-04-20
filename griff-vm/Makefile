CC=gcc
CFLAGS=-O2 -Wall -Wextra `pkg-config --cflags --libs bdw-gc`

.PHONY: main
main: main.o
	$(CC) $(CFLAGS) main.o -o main

.PHONY: main.o
main.o: main.c
	$(CC) -c $(CFLAGS) main.c -o main.o

clean:
	rm main.o main

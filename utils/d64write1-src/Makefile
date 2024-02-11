CFLAGS = -Os -Wall
LDLIBS =
CC = gcc
WCC = x86_64-w64-mingw32-gcc

all: d64write1 # d64write1.exe

win: d64write1.exe

d64write1: d64write1.c *.h
	@echo "=== Building d64write1..."
	@$(CC) $< -o $@ $(CFLAGS)
	@cp $@ ..

d64write1.exe: d64write1.c *.h
	@echo "=== Building d64write1 / Windows x86_64..."
	@$(WCC) $< -o $@
	@cp $@ ..

clean:
	@rm -f d64write1 d64write1.exe \
	       ../d64write1 ../d64write1.exe

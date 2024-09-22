CFLAGS = -Os -Wall
LDLIBS =
CC = gcc
STRIP = strip
WCC = x86_64-w64-mingw32-gcc
WSTRIP = x86_64-w64-mingw32-strip

ifeq ($(OS),Windows_NT)
	PFEXT = .exe
else
	PFEXT = 
endif

all: d64write1 # d64write1.exe

win: d64write1.exe

d64write1: d64write1.c *.h
	@echo "=== Building d64write1..."
	@$(CC) $< -o $@ $(CFLAGS)
	@$(STRIP) $@$(PFEXT)
	@cp $@ ..

d64write1.exe: d64write1.c *.h
	@echo "=== Building d64write1 / Windows x86_64..."
	@$(WCC) $< -o $@
	@$(WSTRIP) $@
	@cp $@ ..

clean:
	@rm -f d64write1 d64write1.exe \
	       ../d64write1 ../d64write1.exe

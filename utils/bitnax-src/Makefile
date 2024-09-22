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

all: bitnax # bitnax.exe bitnax_moff bitnax_moff.exe

win: bitnax.exe

bitnax: lz.c
	@echo "=== Building bitnax..."
	@$(CC) $< -o $@ $(CFLAGS)
	@$(STRIP) $@$(PFEXT)
	@cp $@ ..

bitnax.exe: lz.c
	@echo "=== Building bitnax / Windows x86_64..."
	@$(WCC) $< -o $@
	@$(WSTRIP) $@
	@cp $@ ..

bitnax_moff: lz_moff.c
	@echo "=== Building bitnax_moff..."
	@$(CC) $< -o $@ $(CFLAGS)
	@cp $@ ..

bitnax_moff.exe: lz_moff.c
	@echo "=== Building bitnax_moff / Windows x86_64..."
	@$(WCC) $< -o $@
	@cp $@ ..

clean:
	@rm -f bitnax bitnax.exe bitnax_moff bitnax_moff.exe \
	       ../bitnax ../bitnax.exe ../bitnax_moff ../bitnax_moff.exe

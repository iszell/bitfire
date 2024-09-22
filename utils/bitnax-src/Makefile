CFLAGS = -Os -Wall
LDLIBS =
CC = gcc
WCC = x86_64-w64-mingw32-gcc

all: bitnax # bitnax.exe bitnax_moff bitnax_moff.exe

win: bitnax.exe

bitnax: lz.c
	@echo "=== Building bitnax..."
	@$(CC) $< -o $@ $(CFLAGS)
	@cp $@ ..

bitnax.exe: lz.c
	@echo "=== Building bitnax / Windows x86_64..."
	@$(WCC) $< -o $@
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

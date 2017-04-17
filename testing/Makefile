ACME = acme
ACMEOPT = -f cbm
D64W = ../bitfire/d64write

all: test.d64

test.d64: test.asm Makefile ../bitfire/config.inc
	make -B -C ../bitfire all
	$(ACME) $(ACMEOPT) -o test.prg test.asm
	$(D64W) -c test.d64 -h hello -i world --side 1 \
			-b bitmap1.prg \
			-b bitmap2.prg \
			-s test.prg

clean:
	rm test.prg test.d64 || true
	make -C ../bitfire clean

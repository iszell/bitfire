ACME = acme
ACMEOPT = -f cbm
D64W = ../bitfire/d64write
LZ = ../bitfire/bitnax

all: test.d64

test.d64: test.asm bitmap1.lz bitmap2.lz Makefile ../bitfire/config.inc
	make -B -C ../bitfire all
	$(ACME) $(ACMEOPT) -r test.lst -o test.prg test.asm
	$(D64W) -c test.d64 -h hello -i world --side 1 \
			-b bitmap1.prg \
			-b bitmap2.prg \
			-b bitmap1.lz \
			-b bitmap2.lz \
			-s test.prg

bitmap1.lz :
	$(LZ) --bitfire -o bitmap1.lz bitmap1.prg

bitmap2.lz :
	$(LZ) --bitfire -o bitmap2.lz bitmap2.prg

clean:
	rm test.prg test.d64 || true
	make -C ../bitfire clean

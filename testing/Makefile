ACME = acme
ACMEOPT = -f cbm
D64W = ../bitfire/d64write
LZ = ../bitfire/bitnax

all: test.d64

test.d64: test.asm bitmap1.lz bitmap2.lz bitmap3.lz bitmap4.lz bitmap5.lz Makefile ../bitfire/config.inc
	make -B -C ../bitfire all
	$(ACME) $(ACMEOPT) -r test.lst -o test.prg test.asm
	$(D64W) -c test.d64 -h hello -i world --side 1 \
			-b bitmap1.prg \
			-b bitmap2.prg \
			-b bitmap3.prg \
			-b bitmap4.prg \
			-b bitmap5.prg \
			-b bitmap1.lz \
			-b bitmap2.lz \
			-b bitmap3.lz \
			-b bitmap4.lz \
			-b bitmap5.lz \
			-s test.prg

bitmap%.lz : bitmap%.prg
	$(LZ) --bitfire -o $@ $<

clean:
	rm test.prg test.d64 *.lz || true
	make -C ../bitfire clean

ACME = acme
ACMEOPT = -f cbm
D64W = ../bitfire/d64write
LZ = ../bitfire/bitnax
INST = ../bitfire/installer

all: test.d64

test.d64: $(D64W) $(INST) test.asm bitmap1.lz bitmap2.lz bitmap3.lz bitmap4.lz bitmap5.lz Makefile ../bitfire/config.inc
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


$(D64W) : 
	make -C ../bitfire d64write

$(LZ) : 
	make -C ../bitfire bitnax

$(INST) : force
	make -C ../bitfire installer

bitmap%.lz : bitmap%.prg $(LZ)
	$(LZ) --bitfire -o $@ $<

force:
	@true

clean:
	rm test.prg test.d64 *.lz || true
	make -C ../bitfire clean

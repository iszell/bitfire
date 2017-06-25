ACME = acme
ACMEOPT = -f cbm
D64W = ../bitfire/d64write
LZ = ../bitfire/bitnax
EXO = ../../exomizer/exomizer2/src/exomizer
INST = ../bitfire/installer_plus4_1541dc_swap.prg

all: test_plus4_1541.d64 test_plus4_1551.d64 test_plus4_all.d64 test_c64.d64

test_plus4_1541.d64: test.d64 $(INST_ALL) $(INST) test.asm ../bitfire/config.inc
	$(ACME) $(ACMEOPT) -DTEST_PLUS4=1 -o test_4_1541.prg test.asm
	cp $< $@
	$(D64W) -d $@ -s test_4_1541.prg

test_plus4_1551.d64: test.d64 $(INST_ALL) $(INST) test.asm ../bitfire/config.inc
	$(ACME) $(ACMEOPT) -DTEST_PLUS4=2 -o test_4_1551.prg test.asm
	cp $< $@
	$(D64W) -d $@ -s test_4_1551.prg

test_plus4_all.d64: test.d64 $(INST_ALL) $(INST) test.asm ../bitfire/config.inc
	$(ACME) $(ACMEOPT) -DTEST_PLUS4=3 -o test_4_all.prg test.asm
	$(EXO) sfx basic -t4 -o test_4_all.exo test_4_all.prg
	cp $< $@
	$(D64W) -d $@ -s test_4_all.exo

test_c64.d64: test.d64 $(INST_ALL) $(INST) test.asm	 ../bitfire/config.inc
	$(ACME) $(ACMEOPT) -DTEST_PLUS4=0 -o test_c64.prg test.asm
	$(LZ) --sfx 2061 -o test_c64.lz test_c64.prg
	cp $< $@
	$(D64W) -d $@ -s test_c64.lz

test.d64: $(D64W) bitmap1.lz bitmap2.lz bitmap3.lz bitmap4.lz bitmap5.lz Makefile
	$(D64W) -c test.d64 -h hello -i world --side 1
	make add_files
	make add_lz_files
	make add_files
	make add_lz_files
	make add_files

add_files: 
	$(D64W) -d test.d64 \
			-b bitmap1.prg \
			-b bitmap2.prg \
			-b bitmap3.prg \
			-b bitmap4.prg \
			-b bitmap5.prg

add_lz_files: 
	$(D64W) -d test.d64 \
			-b bitmap1.lz \
			-b bitmap2.lz \
			-b bitmap3.lz \
			-b bitmap4.lz \
			-b bitmap5.lz

$(D64W) : 
	make -C ../bitfire d64write

$(LZ) : 
	make -C ../bitfire bitnax

$(INST) : force
	make -C ../bitfire installers
	
bitmap%.lz : bitmap%.prg $(LZ)
	$(LZ) --bitfire -o $@ $<

force:
	@true

clean:
	rm test*.prg *.d64 *.lz || true
	make -C ../bitfire clean

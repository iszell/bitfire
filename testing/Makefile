ACME = acme
ACMEOPT = -f cbm
D64W = ../bitfire/d64write
LZ = ../bitfire/bitnax
INST = ../bitfire/installer_plus4_1541dc_swap.prg
SAVE = ../bitfire/save_c64.prg

all: test_plus4_1541.d64 test_plus4_1551.d64 test_plus4_multi.d64 test_c64.d64 \
	save_test_c64.d64

test_plus4_1541.d64: test.d64 $(INST_ALL) $(INST) test.asm ../bitfire/config.inc
	$(ACME) $(ACMEOPT) -DTEST_PLUS4=1 -o test_4_1541.prg test.asm
	$(LZ) --sfx 4109 --plus4 -o test_4_1541.lz test_4_1541.prg
	cp $< $@
	$(D64W) -d $@ --boot test_4_1541.lz

test_plus4_1551.d64: test.d64 $(INST_ALL) $(INST) test.asm ../bitfire/config.inc
	$(ACME) $(ACMEOPT) -DTEST_PLUS4=2 -o test_4_1551.prg test.asm
	$(LZ) --sfx 4109 --plus4 -o test_4_1551.lz test_4_1551.prg
	cp $< $@
	$(D64W) -d $@ --boot test_4_1551.lz

test_plus4_multi.d64: test.d64 $(INST_ALL) $(INST) test.asm ../bitfire/config.inc
	$(ACME) $(ACMEOPT) -DTEST_PLUS4=3 -o test_4_multi.prg test.asm
	$(LZ) --sfx 4109 --plus4 -o test_4_multi.lz test_4_multi.prg
	cp $< $@
	$(D64W) -d $@ --boot test_4_multi.lz

test_c64.d64: test.d64 $(INST_ALL) $(INST) test.asm	 ../bitfire/config.inc
	$(ACME) $(ACMEOPT) -DTEST_PLUS4=0 -o test_c64.prg test.asm
	$(LZ) --sfx 2061 -o test_c64.lz test_c64.prg
	cp $< $@
	$(D64W) -d $@ --boot test_c64.lz

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

save_test_c64.d64 : save_test.asm screen_c64.prg $(D64W) $(LZ) $(INST) $(SAVE)
	$(ACME) $(ACMEOPT) -DTEST_PLUS4=0 -o save_test_c64.prg $<
	$(LZ) --sfx 2061 -o save_test_c64.lz save_test_c64.prg
	$(LZ) --bitfire -o save_c64.lz ../bitfire/save_c64.prg
	$(D64W) -c $@ --boot save_test_c64.lz -b save_c64.lz -ts -b screen_c64.prg

$(D64W) : 
	make -C ../bitfire d64write

$(LZ) : 
	make -C ../bitfire bitnax

$(INST) : force
	make -C ../bitfire installers
	
$(SAVE) : force
	make -C ../bitfire save_routines
	
bitmap%.lz : bitmap%.prg $(LZ)
	$(LZ) --bitfire -o $@ $<

force:
	@true

clean:
	rm test*.prg *.d64 *.lz || true
	make -C ../bitfire clean

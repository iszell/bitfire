# BitFire - "Sector Write Test-program" Makefile

ACME = ../../utils/acme
ACMEOPT = -f cbm -r $@.lst
D64W = ../../utils/d64write1
LZ = ../../utils/bitnax
GENINCDIR=../../gen-includes
GENBINDIR=../../gen-binaries

all: savetst-c64.d64 savetst-c264.d64

savetest-c64-multi: savetest-c64-multi.asm savetest.asm $(GENBINDIR)/installer-c64-multi.prg $(GENINCDIR)/loader-c64-41-acme.inc $(GENINCDIR)/saver-c64-41-acme.inc $(ACME) $(LZ)
	@echo "=== Building C64 multi (1541/FSDV) save test..."
	@$(ACME) $(ACMEOPT) -o $@.prg $<
	@$(LZ) --sfx 2061 -o savetst-64.lz $@.prg
	@cp savetst-64.lz $@

savetest-c264-multi: savetest-c264-multi.asm savetest.asm $(GENBINDIR)/installer-c264-multi.prg $(GENINCDIR)/loader-c264-multi-acme.inc $(GENINCDIR)/saver-c264-41multi-acme.inc $(GENINCDIR)/saver-c264-51multi-acme.inc $(ACME) $(LZ)
	@echo "=== Building C264 multi (1541SC/DC/1551/FSDV) save test..."
	@$(ACME) $(ACMEOPT) -o $@.prg $<
	@$(LZ) --sfx 4109 --plus4 -o savetst-264.lz $@.prg
	@cp savetst-264.lz $@

savetst-c64.d64: savetest-c64-multi $(LZ) $(D64W) $(GENBINDIR)/save-resi-c64-41.prg
	@echo "=== Create C64 multi (1541/FSDV) save test disk..."
	@$(LZ) --bitfire -o save-c64-41.lz $(GENBINDIR)/save-resi-c64-41.prg
	@$(D64W) -c $@ --boot savetst-64.lz -ts -b data/screen_c64.prg -b save-c64-41.lz
	@cp $@ ..

savetst-c264.d64: savetest-c264-multi $(LZ) $(D64W) $(GENBINDIR)/save-resi-c264-41multi.prg $(GENBINDIR)/save-resi-c264-51multi.prg
	@echo "=== Create C264 multi (1541/1551; Single/Double Clock) save test disk..."
	@$(LZ) --bitfire -o save-c264-41m.lz $(GENBINDIR)/save-resi-c264-41multi.prg
	@$(LZ) --bitfire -o save-c264-51m.lz $(GENBINDIR)/save-resi-c264-51multi.prg
	@$(D64W) -c $@ --boot savetst-264.lz -ts -b data/screen_plus4.prg -b save-c264-41m.lz -b save-c264-51m.lz
	@cp $@ ..

clean:
	@rm -f savetest-c64-multi.lst savetest-c64-multi.prg savetest-c64-multi \
	       savetest-c264-multi.lst savetest-c264-multi.prg savetest-c264-multi \
	       save*.lz savetst*.d64 ../savetst*.d64

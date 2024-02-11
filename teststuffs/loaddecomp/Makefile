# BitFire - "Load Test-program" Makefile

ACME = ../../utils/acme
ACMEOPT = -f cbm -r $@.lst
D64W = ../../utils/d64write1
LZ = ../../utils/bitnax
GENINCDIR = ../../gen-includes
GENBINDIR = ../../gen-binaries

all: test-c64.d64 test-c264.d64

test-c64-multi: test-c64-multi.asm test.asm $(GENBINDIR)/installer-c64-multi.prg $(GENINCDIR)/loader-c64-multi-acme.inc $(ACME) $(LZ)
	@echo "=== Building C64 multi (1541/FSDV) Loader test..."
	@$(ACME) $(ACMEOPT) -o $@.prg $<
	@$(LZ) --sfx 2061 -o $@.lz $@.prg
	@cp $@.lz $@

test-c264-multi: test-c264-multi.asm test.asm $(GENBINDIR)/installer-c264-multi-sc.prg $(GENINCDIR)/loader-c264-multi-sc-acme.inc $(ACME) $(LZ)
	@echo "=== Building C264 multi (1541SC/1541DC/1551/FSDV) Loader test..."
	@$(ACME) $(ACMEOPT) -o $@.prg $<
	@$(LZ) --sfx 4109 --plus4 -o $@.lz $@.prg
	@cp $@.lz $@

test-c64.d64: test-c64-multi $(D64W) testdisk.d64
	@echo "=== Create C64 multi (1541/FSDV) test disk..."
	@cp testdisk.d64 $@
	@$(D64W) -d $@ --boot test-c64-multi
	@cp $@ ..

test-c264.d64: test-c264-multi $(D64W) testdisk.d64
	@echo "=== Create C264 multi (1541/1551; Single/Double Clock) test disk..."
	@cp testdisk.d64 $@
	@$(D64W) -d $@ --boot test-c264-multi
	@cp $@ ..

testdisk.d64: data/bitmap1.lz data/bitmap2.lz data/bitmap3.lz data/bitmap4.lz data/bitmap5.lz $(D64W)
	@rm -f testdisk-temp.d64
	@$(D64W) -c testdisk-temp.d64 -h hello -i world --side 1
	@make add_files
	@make add_lz_files
	@make add_files
	@make add_lz_files
	@make add_files
	@mv testdisk-temp.d64 $@

add_files:
	@$(D64W) -d testdisk-temp.d64 -b data/bitmap1.prg \
				      -b data/bitmap2.prg \
				      -b data/bitmap3.prg \
				      -b data/bitmap4.prg \
				      -b data/bitmap5.prg

add_lz_files:
	@$(D64W) -d testdisk-temp.d64 -b data/bitmap1.lz \
				      -b data/bitmap2.lz \
				      -b data/bitmap3.lz \
				      -b data/bitmap4.lz \
				      -b data/bitmap5.lz

data/bitmap%.lz: data/bitmap%.prg $(LZ)
	@$(LZ) --bitfire -o $@ $<

clean:
	@rm -f test-c64-multi test-c64-multi.prg test-c64-multi.lz test-c64-multi.lst \
	       test-c264-multi test-c264-multi.prg test-c264-multi.lz test-c264-multi.lst \
	       data/bitmap?.lz \
	       test*.d64 ../test-c*.d64

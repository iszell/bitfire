# BitFire - "Load Speed benchmark Test-program" Makefile

ACME = ../../utils/acme
ACMEOPT = -f cbm -r $@.lst
D64W = ../../utils/d64write1
LZ = ../../utils/bitnax
GENINCDIR=../../gen-includes
GENBINDIR=../../gen-binaries

all: spd-c64.d64 spd-c264.d64

spdtst-c64-multi: spdtst-c64-multi.asm spdtst.asm $(GENBINDIR)/installer-c64-multi.prg $(GENINCDIR)/loader-c64-multi-acme.inc $(ACME) $(LZ)
	@echo "=== Building C64 multi (1541/FSDV) Loader speed test..."
	@$(ACME) $(ACMEOPT) -o $@.prg $<
	@$(LZ) --sfx 2061 -o spdtst-64.lz $@.prg
	@cp spdtst-64.lz $@

spdtst-c264-multi: spdtst-c264-multi.asm spdtst.asm $(GENBINDIR)/installer-c264-multi-sc.prg $(GENINCDIR)/loader-c264-multi-sc-acme.inc $(ACME) $(LZ)
	@echo "=== Building C264 multi (1541SC+DC/1551/FSDV) Loader speed test..."
	@$(ACME) $(ACMEOPT) -o $@.prg $<
	@$(LZ) --sfx 4109 --plus4 -o spdtst-264.lz $@.prg
	@cp spdtst-264.lz $@

spd-c64.d64: spdtst-c64-multi spdtstdisk.d64 $(D64W)
	@echo "=== Create C64 multi (1541/FSDV) Loader speed test disk..."
	@cp spdtstdisk.d64 $@
	@$(D64W) -d $@ --boot spdtst-64.lz
	@cp $@ ..

spd-c264.d64: spdtst-c264-multi spdtstdisk.d64 $(D64W)
	@echo "=== Create C264 multi (1541SC+DC/1551/FSDV) Loader speed test disk..."
	@cp spdtstdisk.d64 $@
	@$(D64W) -d $@ --boot spdtst-264.lz
	@cp $@ ..

spdtstdisk.d64: data/emptyblock data/random-32k $(D64W)
	@rm -f spdtstdisk-temp.d64
	@$(D64W) -c spdtstdisk-temp.d64 -h speedtest -i disk --side 1
	@$(D64W) -d spdtstdisk-temp.d64 -b data/emptyblock \
					-b data/random-32k \
					-b data/random-32k \
					-b data/random-32k \
					-b data/random-32k \
					-b data/random-32k
	@mv spdtstdisk-temp.d64 $@

clean:
	@rm -f spdtst-c64-multi spdtst-c64-multi.prg spdtst-c64-multi.lz spdtst-c64-multi.lst spdtst-64.lz \
	       spdtst-c264-multi spdtst-c264-multi.prg spdtst-c264-multi.lz spdtst-c264-multi.lst spdtst-264.lz \
	       spdtst*.d64 spd-c*.d64 ../spd-c*.d64

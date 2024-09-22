# BitFire - "Installer" Makefile

ACME = ../utils/acme
ACMEOPT = -f cbm -r $@.lst
GENINCDIR=../gen-includes
GENBINDIR=../gen-binaries

define gen_includes
	@grep "bitfire_*" $(1).tlb | sed "s/\s*;\s*.*//g" | tr -d "\t" | sed 's/\$$\([0-9a-f]\{3\}$$\)/\$$0\1/' | sed "s/\= / \= /" > $(1).inc
	@grep "link_*" $(1).tlb | sed "s/\s*;\s*.*//g" | tr -d "\t" | sed 's/\$$\([0-9a-f]\{3\}$$\)/\$$0\1/' | sed "s/\= / \= /" >> $(1).inc
	@grep "BITFIRE_*" $(1).tlb | sed "s/\s*;\s*.*//g" | tr -d "\t" | sed 's/\$$\([0-9a-f]\{3\}$$\)/\$$0\1/' | sed "s/\= / \= /" >> $(1).inc
	@cp $(1).inc $(2)-acme.inc
	@echo "if def BITFIRE.ISINCLUDED == 0" > $(2)-c6510.inc
	@cat $(2)-acme.inc | sed "s/\(.*\)/const \\1/" >> $(2)-c6510.inc
	@echo "endif" >> $(2)-c6510.inc
	@echo ".importonce" > $(2)-kickass.inc
	@#cat $(2)-acme.inc | grep "bitfire" | sed "s/bitfire/\.var bitfire/" >> $(2)-kickass.inc
	@#cat $(2)-acme.inc | grep "BITFIRE" | sed "s/BITFIRE/\.var BITFIRE/" >> $(2)-kickass.inc
	@cat $(2)-acme.inc | sed "s/\(.*\)/\.var \\1/" >> $(2)-kickass.inc
	@#echo "}" >> loader_kickass.inc
	@mkdir -p $(GENINCDIR)
	@cp $(2)-*.inc $(GENINCDIR)
endef

all: installer-c64-41 installer-c64-multi installer-c264-41sc installer-c264-41dc installer-c264-51dc installer-c264-multi installer-c264-multi-sc

installer-c64-41: installer-c64-41.asm installer.asm ../drive/bootstrap-1541 ../drive/drivecode-1541 ../resident/resident-c64-41 ../config/config.inc $(ACME)
	@echo "=== Building C64 1541 Installer..."
	@$(ACME) $(ACMEOPT) -l $@.tlb -o $@.prg $<
	@$(call gen_includes, $@, loader-c64-41)
	@cp $@.prg $@
	@mkdir -p $(GENBINDIR)
	@cp $@ $(GENBINDIR)/$@.prg

installer-c64-multi: installer-c64-multi.asm installer.asm ../drive/bootstrap-1541 ../drive/drivecode-1541 ../drive/bootstrap-fsdv ../drive/drivecode-fsdv ../resident/resident-c64-41 ../resident/fsdv-1bitpatch-c64 ../config/config.inc $(ACME)
	@echo "=== Building C64 multi (1541/FSDV) Installer..."
	@$(ACME) $(ACMEOPT) -l $@.tlb -o $@.prg $<
	@$(call gen_includes, $@, loader-c64-multi)
	@cp $@.prg $@
	@mkdir -p $(GENBINDIR)
	@cp $@ $(GENBINDIR)/$@.prg

installer-c264-41sc: installer-c264-41sc.asm installer.asm ../drive/bootstrap-1541 ../drive/drivecode-1541 ../resident/resident-c264-41sc ../config/config.inc $(ACME)
	@echo "=== Building C264 1541 Single Clock only Installer..."
	@$(ACME) $(ACMEOPT) -l $@.tlb -o $@.prg $<
	@$(call gen_includes, $@, loader-c264-41sc)
	@cp $@.prg $@
	@mkdir -p $(GENBINDIR)
	@cp $@ $(GENBINDIR)/$@.prg

installer-c264-41dc: installer-c264-41dc.asm installer.asm ../drive/bootstrap-1541 ../drive/drivecode-1541 ../resident/resident-c264-41dc ../config/config.inc $(ACME)
	@echo "=== Building C264 1541 Double Clock only Installer..."
	@$(ACME) $(ACMEOPT) -l $@.tlb -o $@.prg $<
	@$(call gen_includes, $@, loader-c264-41dc)
	@cp $@.prg $@
	@mkdir -p $(GENBINDIR)
	@cp $@ $(GENBINDIR)/$@.prg

installer-c264-51dc: installer-c264-51dc.asm installer.asm ../drive/bootstrap-1551 ../drive/drivecode-1551 ../resident/resident-c264-51dc ../config/config.inc $(ACME)
	@echo "=== Building C264 1551 Double Clock only Installer..."
	@$(ACME) $(ACMEOPT) -l $@.tlb -o $@.prg $<
	@$(call gen_includes, $@, loader-c264-51dc)
	@cp $@.prg $@
	@mkdir -p $(GENBINDIR)
	@cp $@ $(GENBINDIR)/$@.prg

installer-c264-multi: installer-c264-multi.asm installer.asm ../drive/bootstrap-1541 ../drive/drivecode-1541 ../drive/bootstrap-1551 ../drive/drivecode-1551 ../drive/bootstrap-fsdv ../drive/drivecode-fsdv ../resident/resi-checkbincomp.bin ../resident/resident-c264-41db ../resident/resident-c264-51db ../resident/fsdv-1bitpatch-c264 ../config/config.inc $(ACME)
	@echo "=== Building C264 Multi (1541DC, 1551, FSDV) Installer..."
	@$(ACME) $(ACMEOPT) -l $@.tlb -o $@.prg $<
	@$(call gen_includes, $@, loader-c264-multi)
	@cp $@.prg $@
	@mkdir -p $(GENBINDIR)
	@cp $@ $(GENBINDIR)/$@.prg

installer-c264-multi-sc: installer-c264-multi-sc.asm installer.asm ../drive/bootstrap-1541 ../drive/drivecode-1541 ../drive/bootstrap-1551 ../drive/drivecode-1551 ../drive/bootstrap-fsdv ../drive/drivecode-fsdv ../resident/resi-checkbincomp.bin ../resident/resident-c264-41sb ../resident/swaprecv-c264-41sb ../resident/resident-c264-41db ../resident/swaprecv-c264-41db ../resident/resident-c264-51db ../resident/swaprecv-c264-51db ../resident/fsdv-1bitpatch-c264 ../config/config.inc $(ACME)
	@echo "=== Building C264 Multi (1541SC/DC, 1551, FSDV) Installer..."
	@$(ACME) $(ACMEOPT) -l $@.tlb -o $@.prg $<
	@$(call gen_includes, $@, loader-c264-multi-sc)
	@cp $@.prg $@
	@mkdir -p $(GENBINDIR)
	@cp $@ $(GENBINDIR)/$@.prg

clean:
	@rm -f installer-c64-41 installer-c64-41.prg installer-c64-41.tlb installer-c64-41.inc installer-c64-41.lst $(GENBINDIR)/installer-c64-41.prg \
	       installer-c64-multi installer-c64-multi.prg installer-c64-multi.tlb installer-c64-multi.inc installer-c64-multi.lst $(GENBINDIR)/installer-c64-multi.prg \
	       installer-c264-41sc installer-c264-41sc.prg installer-c264-41sc.tlb installer-c264-41sc.inc installer-c264-41sc.lst $(GENBINDIR)/installer-c264-41sc.prg \
	       installer-c264-41dc installer-c264-41dc.prg installer-c264-41dc.tlb installer-c264-41dc.inc installer-c264-41dc.lst $(GENBINDIR)/installer-c264-41dc.prg \
	       installer-c264-51dc installer-c264-51dc.prg installer-c264-51dc.tlb installer-c264-51dc.inc installer-c264-51dc.lst $(GENBINDIR)/installer-c264-51dc.prg \
	       installer-c264-multi installer-c264-multi.prg installer-c264-multi.tlb installer-c264-multi.inc installer-c264-multi.lst $(GENBINDIR)/installer-c264-multi.prg \
	       installer-c264-multi-sc installer-c264-multi-sc.prg installer-c264-multi-sc.tlb installer-c264-multi-sc.inc installer-c264-multi-sc.lst $(GENBINDIR)/installer-c264-multi-sc.prg \
	       loader-*-acme.inc loader-*-c6510.inc loader-*-kickass.inc \
	       $(GENINCDIR)/loader-*-acme.inc $(GENINCDIR)/loader-*-c6510.inc $(GENINCDIR)/loader-*-kickass.inc
	if [ -d $(GENBINDIR) ]; then rmdir --ignore-fail-on-non-empty $(GENBINDIR); fi
	if [ -d $(GENINCDIR) ]; then rmdir --ignore-fail-on-non-empty $(GENINCDIR); fi

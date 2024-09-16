# BitFire - "SectorWriter Codes" Makefile

ACME = ../utils/acme
ACMEOPTD = -f plain -r $@.lst
ACMEOPTR = -f cbm -r $@.lst
GENINCDIR=../gen-includes
GENBINDIR=../gen-binaries

define gend_includes
	@grep $(2) $(1).tlb | sed "s/\s*;\s*.*//g" | tr -d "\t" | sed 's/\$$\([0-9a-f]\{3\}$$\)/\$$0\1/' | sed "s/\= / \= /" > $(1).inc
endef

define genr_includes
	@grep $(3) $(1).tlb | sed "s/\s*;\s*.*//g" | tr -d "\t" | sed 's/\$$\([0-9a-f]\{3\}$$\)/\$$0\1/' | sed "s/\= / \= /" > $(1).inc
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

all: save-drive-41 save-resi-c64-41 save-resi-c264-41sc save-resi-c264-41dc save-resi-c264-41multi save-drive-51 save-resi-c264-51dc save-resi-c264-51multi

save-drive-41: save-drive-41.asm save-drivecode.asm ../drive/drivecode-1541.inc ../config/config.inc $(ACME)
	@echo "=== Building 1541 save drivecode..."
	@$(ACME) $(ACMEOPTD) -l $@.tlb -o $@.bin $<
	@$(call gend_includes, $@, "drv_*")
	@cp $@.bin $@

save-drive-51: save-drive-51.asm save-drivecode.asm ../drive/drivecode-1551.inc ../config/config.inc $(ACME)
	@echo "=== Building 1551 save drivecode..."
	@$(ACME) $(ACMEOPTD) -l $@.tlb -o $@.bin $<
	@$(call gend_includes, $@, "drv_*")
	@cp $@.bin $@

save-resi-c64-41: save-resi-c64-41.asm save-resident.asm save-drive-41 ../config/config.inc $(ACME)
	@echo "=== Building C64 1541 save resident..."
	@$(ACME) $(ACMEOPTR) -l $@.tlb -o $@.bin $<
	@$(call genr_includes, $@, saver-c64-41, "bitfire_save_*")
	@cp $@.bin $@
	@mkdir -p $(GENBINDIR)
	@cp $@ $(GENBINDIR)/$@.prg

save-resi-c264-41sc: save-resi-c264-41sc.asm save-resident.asm save-drive-41 ../config/config.inc $(ACME)
	@echo "=== Building C264 1541 Single Clock only save resident..."
	@$(ACME) $(ACMEOPTR) -l $@.tlb -o $@.bin $<
	@$(call genr_includes, $@, saver-c264-41sc, "bitfire_save_*")
	@cp $@.bin $@
	@cp $@ $(GENBINDIR)/$@.prg

save-resi-c264-41dc: save-resi-c264-41dc.asm save-resident.asm save-drive-41 ../config/config.inc $(ACME)
	@echo "=== Building C264 1541 Double Clock only save resident..."
	@$(ACME) $(ACMEOPTR) -l $@.tlb -o $@.bin $<
	@$(call genr_includes, $@, saver-c264-41dc, "bitfire_save_*")
	@cp $@.bin $@
	@cp $@ $(GENBINDIR)/$@.prg

save-resi-c264-51dc: save-resi-c264-51dc.asm save-resident.asm save-drive-51 ../config/config.inc $(ACME)
	@echo "=== Building C264 1551 Double Clock only save resident..."
	@$(ACME) $(ACMEOPTR) -l $@.tlb -o $@.bin $<
	@$(call genr_includes, $@, saver-c264-51dc, "bitfire_save_*")
	@cp $@.bin $@
	@cp $@ $(GENBINDIR)/$@.prg

save-resi-c264-41multi: save-resi-c264-41multi.asm save-resident.asm save-drive-41 ../config/config.inc $(ACME)
	@echo "=== Building C264 1541 multi (Single/Double Clock) save resident..."
	@$(ACME) $(ACMEOPTR) -l $@.tlb -o $@.bin $<
	@$(call genr_includes, $@, saver-c264-41multi, "bitfire_save_*")
	@cp $@.bin $@
	@cp $@ $(GENBINDIR)/$@.prg

save-resi-c264-51multi: save-resi-c264-51multi.asm save-resident.asm save-drive-51 ../config/config.inc $(ACME)
	@echo "=== Building C264 1551 multi save resident..."
	@$(ACME) $(ACMEOPTR) -l $@.tlb -o $@.bin $<
	@$(call genr_includes, $@, saver-c264-51multi, "bitfire_save_*")
	@cp $@.bin $@
	@cp $@ $(GENBINDIR)/$@.prg

clean:
	@rm -f save-drive-41 save-drive-41.bin save-drive-41.tlb save-drive-41.inc save-drive-41.lst \
	       save-drive-51 save-drive-51.bin save-drive-51.tlb save-drive-51.inc save-drive-51.lst \
	       save-resi-c64-41 save-resi-c64-41.bin save-resi-c64-41.tlb save-resi-c64-41.inc save-resi-c64-41.lst $(GENBINDIR)/save-resi-c64-41.prg \
	       save-resi-c264-41sc save-resi-c264-41sc.bin save-resi-c264-41sc.tlb save-resi-c264-41sc.inc save-resi-c264-41sc.lst $(GENBINDIR)/save-resi-c264-41sc.prg \
	       save-resi-c264-41dc save-resi-c264-41dc.bin save-resi-c264-41dc.tlb save-resi-c264-41dc.inc save-resi-c264-41dc.lst $(GENBINDIR)/save-resi-c264-41dc.prg \
	       save-resi-c264-51dc save-resi-c264-51dc.bin save-resi-c264-51dc.tlb save-resi-c264-51dc.inc save-resi-c264-51dc.lst $(GENBINDIR)/save-resi-c264-51dc.prg \
	       save-resi-c264-41multi save-resi-c264-41multi.bin save-resi-c264-41multi.tlb save-resi-c264-41multi.inc save-resi-c264-41multi.lst $(GENBINDIR)/save-resi-c264-41multi.prg \
	       save-resi-c264-51multi save-resi-c264-51multi.bin save-resi-c264-51multi.tlb save-resi-c264-51multi.inc save-resi-c264-51multi.lst $(GENBINDIR)/save-resi-c264-51multi.prg \
	       saver-*-acme.inc saver-*-c6510.inc saver-*-kickass.inc \
	       $(GENINCDIR)/saver-*-acme.inc $(GENINCDIR)/saver-*-c6510.inc $(GENINCDIR)/saver-*-kickass.inc

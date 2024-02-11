# BitFire - "Resident Codes" Makefile

ACME = ../utils/acme
ACMEOPT = -f plain -r $@.lst

define gen_includes
	@grep "bitfire_*" $(1).tlb | sed "s/\s*;\s*.*//g" | tr -d "\t" | sed 's/\$$\([0-9a-f]\{3\}$$\)/\$$0\1/' | sed "s/\= / \= /" > $(1).inc
	@grep "link_*" $(1).tlb | sed "s/\s*;\s*.*//g" | tr -d "\t" | sed 's/\$$\([0-9a-f]\{3\}$$\)/\$$0\1/' | sed "s/\= / \= /" >> $(1).inc
	@grep "BITFIRE_*" $(1).tlb | sed "s/\s*;\s*.*//g" | tr -d "\t" | sed 's/\$$\([0-9a-f]\{3\}$$\)/\$$0\1/' | sed "s/\= / \= /" >> $(1).inc
endef

define gen_cmpincludes
	@grep "bitfire_*" $(1).tlb | sed "s/\s*;\s*.*//g" | tr -d "\t" | sed 's/\$$\([0-9a-f]\{3\}$$\)/\$$0\1/' | sed "s/\= / \= /" > $(1).incc
	@grep "link_*" $(1).tlb | sed "s/\s*;\s*.*//g" | tr -d "\t" | sed 's/\$$\([0-9a-f]\{3\}$$\)/\$$0\1/' | sed "s/\= / \= /" >> $(1).incc
endef

define gen_fsdvpincludes
	@grep "fsdvp_*" $(1).tlb | sed "s/\s*;\s*.*//g" | tr -d "\t" | sed 's/\$$\([0-9a-f]\{3\}$$\)/\$$0\1/' | sed "s/\= / \= /" > $(1).incp
endef

all: resident-c64-41 resident-c264-41sc resident-c264-41dc resident-c264-51dc resident-c264-41sb resident-c264-41db resident-c264-51db resi-checkbincomp fsdv-1bitpatch-c64 fsdv-1bitpatch-c264 swaprecv-c264-41sb swaprecv-c264-41db swaprecv-c264-51db

resident-c64-41: resident-c64-41.asm resident.asm ../config/config.inc ../config/music.inc $(ACME)
	@echo "=== Building C64 1541 resident..."
	@$(ACME) $(ACMEOPT) -l $@.tlb -o $@.bin $<
	@$(call gen_includes, $@)
	@$(call gen_fsdvpincludes, $@)
	@cp $@.bin $@

resident-c264-41sc: resident-c264-41sc.asm resident.asm ../config/config.inc ../config/music.inc $(ACME)
	@echo "=== Building C264 1541 Single Clock Only resident..."
	@$(ACME) $(ACMEOPT) -l $@.tlb -o $@.bin $<
	@$(call gen_includes, $@)
	@cp $@.bin $@

resident-c264-41dc: resident-c264-41dc.asm resident.asm ../config/config.inc ../config/music.inc $(ACME)
	@echo "=== Building C264 1541 Double Clock Only resident..."
	@$(ACME) $(ACMEOPT) -l $@.tlb -o $@.bin $<
	@$(call gen_includes, $@)
	@cp $@.bin $@

resident-c264-51dc: resident-c264-51dc.asm resident.asm ../config/config.inc ../config/music.inc $(ACME)
	@echo "=== Building C264 1551 Only resident..."
	@$(ACME) $(ACMEOPT) -l $@.tlb -o $@.bin $<
	@$(call gen_includes, $@)
	@cp $@.bin $@

resident-c264-41sb: resident-c264-41sb.asm resident.asm ../config/config.inc ../config/music.inc $(ACME)
	@echo "=== Building C264 1541 Single Clock BinaryComp resident..."
	@$(ACME) $(ACMEOPT) -l $@.tlb -o $@.bin $<
	@$(call gen_includes, $@)
	@$(call gen_cmpincludes, $@)
	@cp $@.bin $@

resident-c264-41db: resident-c264-41db.asm resident.asm ../config/config.inc ../config/music.inc $(ACME)
	@echo "=== Building C264 1541 Double Clock BinaryComp resident..."
	@$(ACME) $(ACMEOPT) -l $@.tlb -o $@.bin $<
	@$(call gen_includes, $@)
	@$(call gen_cmpincludes, $@)
	@$(call gen_fsdvpincludes, $@)
	@cp $@.bin $@

resident-c264-51db: resident-c264-51db.asm resident.asm ../config/config.inc ../config/music.inc $(ACME)
	@echo "=== Building C264 1551 BinaryComp resident..."
	@$(ACME) $(ACMEOPT) -l $@.tlb -o $@.bin $<
	@$(call gen_includes, $@)
	@$(call gen_cmpincludes, $@)
	@cp $@.bin $@

resi-checkbincomp: resi-checkbincomp.asm resident-c264-41sb resident-c264-41db resident-c264-51db resident-c264-41sb.incc resident-c264-41db.incc resident-c264-51db.incc $(ACME)
	@echo "=== Checking BinaryComp symbols..."
	@$(ACME) $(ACMEOPT) -o $@.bin $<
	@cp $@.bin $@

fsdv-1bitpatch-c64: fsdv-1bitpatch-c64.asm fsdv-1bitpatch.asm resident-c64-41 ../config/config.inc
	@echo "=== Building C64 FSDV 1bit patch..."
	@$(ACME) $(ACMEOPT) -l $@.tlb -o $@.bin $<
	@cp $@.bin $@

fsdv-1bitpatch-c264: fsdv-1bitpatch-c264.asm fsdv-1bitpatch.asm resident-c264-41db ../config/config.inc
	@echo "=== Building C264 FSDV 1bit patch..."
	@$(ACME) $(ACMEOPT) -l $@.tlb -o $@.bin $<
	@cp $@.bin $@

swaprecv-c264-41sb: swaprecv-c264-41sb.asm swaprecv-c264.asm resi-checkbincomp resident-c264-41sb resident-c264-41db ../config/config.inc $(ACME)
	@echo "=== Building C264 1541 Single-Double swapper..."
	@$(ACME) $(ACMEOPT) -l $@.tlb -o $@.bin $<
	@grep "swapper_*" $@.tlb | sed "s/\s*;\s*.*//g" | tr -d "\t" | sed 's/\$$\([0-9a-f]\{3\}$$\)/\$$0\1/' | sed "s/\= / \= /" > $@.inc
	@cp $@.bin $@

swaprecv-c264-41db: swaprecv-c264-41db.asm swaprecv-c264.asm resi-checkbincomp resident-c264-41sb resident-c264-41db ../config/config.inc $(ACME)
	@echo "=== Building C264 1541 Double-Single swapper..."
	@$(ACME) $(ACMEOPT) -l $@.tlb -o $@.bin $<
	@grep "swapper_*" $@.tlb | sed "s/\s*;\s*.*//g" | tr -d "\t" | sed 's/\$$\([0-9a-f]\{3\}$$\)/\$$0\1/' | sed "s/\= / \= /" > $@.inc
	@cp $@.bin $@

swaprecv-c264-51db: swaprecv-c264-51db.asm swaprecv-c264.asm resi-checkbincomp resident-c264-51db ../config/config.inc $(ACME)
	@echo "=== Building C264 1551 swapper..."
	@$(ACME) $(ACMEOPT) -l $@.tlb -o $@.bin $<
	@grep "swapper_*" $@.tlb | sed "s/\s*;\s*.*//g" | tr -d "\t" | sed 's/\$$\([0-9a-f]\{3\}$$\)/\$$0\1/' | sed "s/\= / \= /" > $@.inc
	@cp $@.bin $@

clean:
	@rm -f resident-c64-41 resident-c64-41.bin resident-c64-41.tlb resident-c64-41.inc resident-c64-41.incp resident-c64-41.lst \
	       resident-c264-41sc resident-c264-41sc.bin resident-c264-41sc.tlb resident-c264-41sc.inc resident-c264-41sc.lst \
	       resident-c264-41dc resident-c264-41dc.bin resident-c264-41dc.tlb resident-c264-41dc.inc resident-c264-41dc.lst \
	       resident-c264-51dc resident-c264-51dc.bin resident-c264-51dc.tlb resident-c264-51dc.inc resident-c264-51dc.lst \
	       resident-c264-41sb resident-c264-41sb.bin resident-c264-41sb.tlb resident-c264-41sb.inc resident-c264-41sb.incc resident-c264-41sb.lst \
	       resident-c264-41db resident-c264-41db.bin resident-c264-41db.tlb resident-c264-41db.inc resident-c264-41db.incc resident-c264-41db.incp resident-c264-41db.lst \
	       resident-c264-51db resident-c264-51db.bin resident-c264-51db.tlb resident-c264-51db.inc resident-c264-51db.incc resident-c264-51db.lst \
	       resi-checkbincomp resi-checkbincomp.bin resi-checkbincomp.lst \
	       fsdv-1bitpatch-c64 fsdv-1bitpatch-c64.bin fsdv-1bitpatch-c64.tlb fsdv-1bitpatch-c64.lst \
	       fsdv-1bitpatch-c264 fsdv-1bitpatch-c264.bin fsdv-1bitpatch-c264.tlb fsdv-1bitpatch-c264.lst \
	       swaprecv-c264-41sb swaprecv-c264-41sb.bin swaprecv-c264-41sb.tlb swaprecv-c264-41sb.inc swaprecv-c264-41sb.lst \
	       swaprecv-c264-41db swaprecv-c264-41db.bin swaprecv-c264-41db.tlb swaprecv-c264-41db.inc swaprecv-c264-41db.lst \
	       swaprecv-c264-51db swaprecv-c264-51db.bin swaprecv-c264-51db.tlb swaprecv-c264-51db.inc swaprecv-c264-51db.lst

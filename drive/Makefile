# BitFire - "Drive Codes" Makefile

ACME = ../utils/acme
ACMEOPT = -f plain -r $@.lst

define gen_includes
	@grep $(2) $(1).tlb | sed "s/\s*;\s*.*//g" | tr -d "\t" | sed 's/\$$\([0-9a-f]\{3\}$$\)/\$$0\1/' | sed "s/\= / \= /" > $(1).inc
endef

default: drivecode-1541 bootstrap-1541 drivecode-1551 bootstrap-1551

fsdv: drivecode-fsdv bootstrap-fsdv

drivecode-1541: drivecode-1541.asm drivecode.asm ../config/config.inc $(ACME)
	@echo "=== Building 1541 drivecode..."
	@$(ACME) $(ACMEOPT) -l $@.tlb -o $@.bin $<
	@$(call gen_includes, $@, "drivecode_*")
	@cp $@.bin $@

drivecode-1551: drivecode-1551.asm drivecode.asm ../config/config.inc $(ACME)
	@echo "=== Building 1551 drivecode..."
	@$(ACME) $(ACMEOPT) -l $@.tlb -o $@.bin $<
	@$(call gen_includes, $@, "drivecode_*")
	@cp $@.bin $@

drivecode-fsdv: drivecode-fsdv.asm drivecode.asm vcpumacros-acme.asm ../config/config.inc $(ACME)
	@echo "=== Building FlexSD drivecode..."
	@$(ACME) $(ACMEOPT) -l $@.tlb -o $@.bin $<
	@$(call gen_includes, $@, "drivecode_*")
	@cp $@.bin $@

bootstrap-1541: bootstrap-1541.asm bootstrap.asm drivecode-1541.tlb ../config/config.inc $(ACME)
	@echo "=== Building 1541 bootstrap..."
	@$(ACME) $(ACMEOPT) -l $@.tlb -o $@.bin $<
	@$(call gen_includes, $@, "bootstrap_*")
	@cp $@.bin $@

bootstrap-1551: bootstrap-1551.asm bootstrap.asm drivecode-1551.tlb ../config/config.inc $(ACME)
	@echo "=== Building 1551 bootstrap..."
	@$(ACME) $(ACMEOPT) -l $@.tlb -o $@.bin $<
	@$(call gen_includes, $@, "bootstrap_*")
	@cp $@.bin $@

bootstrap-fsdv: bootstrap-fsdv.asm bootstrap.asm drivecode-fsdv.tlb vcpumacros-acme.asm ../config/config.inc $(ACME)
	@echo "=== Building FlexSD bootstrap..."
	@$(ACME) $(ACMEOPT) -l $@.tlb -o $@.bin $<
	@$(call gen_includes, $@, "bootstrap_*")
	@cp $@.bin $@

clean:
	@rm -f drivecode-1541 drivecode-1541.bin drivecode-1541.tlb drivecode-1541.inc drivecode-1541.lst \
	       drivecode-1551 drivecode-1551.bin drivecode-1551.tlb drivecode-1551.inc drivecode-1551.lst \
	       drivecode-fsdv drivecode-fsdv.bin drivecode-fsdv.tlb drivecode-fsdv.inc drivecode-fsdv.lst \
	       bootstrap-1541 bootstrap-1541.bin bootstrap-1541.tlb bootstrap-1541.inc bootstrap-1541.lst \
	       bootstrap-1551 bootstrap-1551.bin bootstrap-1551.tlb bootstrap-1551.inc bootstrap-1551.lst \
	       bootstrap-fsdv bootstrap-fsdv.bin bootstrap-fsdv.tlb bootstrap-fsdv.inc bootstrap-fsdv.lst

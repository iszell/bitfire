ACME = acme
ACMEOPT = -f cbm
CFLAGS = -Os -s -Wall
LDLIBS =
CCL = gcc
CCW = x86_64-w64-mingw32-gcc

ACME_DEFS = -DBITFIRE_PLATFORM=$(PLATFORM) -DBITFIRE_PLUS4_MODE=$(PLUS4_MODE) -DBF_PLUS4_BINCOMP=$(PLUS4_BINCOMP)

SAVE_C64_ADDR = "0400"           #03e0
SAVE_PLUS4_1551_ADDR = "0400"    #03d0
SAVE_PLUS4_MULTI_ADDR = "0480"   #0470

all: bitnax d64write bitnax.exe d64write.exe installer.prg installers save_routines 

d64write: d64write.c *.h
	@echo "Building d64write..."
	@$(CCL) $< -o $@ $(CFLAGS)

d64write.exe: d64write.c *.h
	@echo "Building d64write.exe..."
	@$(CCW) $< -o $@ $(CFLAGS)

bitnax: lz.c
	@echo "Building bitnax..."
	@${CCL} $< -o $@ ${CFLAGS}

bitnax.exe: lz.c
	@echo "Building bitnax.exe..."
	@${CCW} $< -o $@ ${CFLAGS}

installer.prg: installer.asm drivecode.asm request_disc.asm resident.asm detect.asm config.inc Makefile resident_minimal_zp.asm
	@echo -n "Building resident part ->"
	@$(ACME) $(ACMEOPT) -o resident -l resident_labels.txt resident.asm
	@printf " Size: $$%04x\n" $$((`wc -c < resident` - 2))
	@echo "Creating loader_acme.inc..."
	@#fetch all global labels for resident part
	@grep "bitfire_*" resident_labels.txt | sed "s/\s*;\s*.*//g" | tr -d "\t" | sed 's/\$$\([0-9a-f]\{3\}$$\)/\$$0\1/' | sed "s/\= / \= /" > loader_acme.inc
	@grep "link_*" resident_labels.txt | sed "s/\s*;\s*.*//g" | tr -d "\t" | sed 's/\$$\([0-9a-f]\{3\}$$\)/\$$0\1/' | sed "s/\= / \= /" >> loader_acme.inc
	@#fetch all config labels
	@grep "BITFIRE_*" resident_labels.txt | sed "s/\s*;\s*.*//g" | tr -d "\t" | sed 's/\$$\([0-9a-f]\{3\}$$\)/\$$0\1/' | sed "s/\= / \= /" >> loader_acme.inc
	@echo "Creating loader_c6510.inc..."
	@#convert to c6510 format (add const, wipe out underscores and add prefix with .)
	@echo "if def BITFIRE.ISINCLUDED == 0" > loader_c6510.inc
	@cat loader_acme.inc | sed "s/\(.*\)/const \\1/" >> loader_c6510.inc
	@echo "endif" >> loader_c6510.inc
	@echo ".importonce" > loader_kickass.inc
	@#cat loader_acme.inc | grep "bitfire" | sed "s/bitfire/\.var bitfire/" >> loader_kickass.inc
	@#cat loader_acme.inc | grep "BITFIRE" | sed "s/BITFIRE/\.var BITFIRE/" >> loader_kickass.inc
	@echo "Creating loader_kickass.inc..."
	@cat loader_acme.inc | sed "s/\(.*\)/\.var \\1/" >> loader_kickass.inc
	@#echo "}" >> loader_kickass.inc
	@echo "Building installer..."
	@$(ACME) $(ACMEOPT) -o installer.prg installer.asm
	@rm resident_labels.txt

#This is the parametrized target for building the resident, please note $(ACME_DEFS).
resident$(SUFF): resident.asm config.inc Makefile resident_minimal_zp.asm
	@echo "----------------------------------------------------------------------------------------"
	@echo "Target: $(@)"
	@echo -n "Building resident part (platform: $(PLATFORM); receiver: $(PLUS4_MODE); bincomp: $(PLUS4_BINCOMP)) ->"
	@$(ACME) $(ACMEOPT) $(ACME_DEFS) -o resident -l resident_labels.txt resident.asm
	@printf " Size: $$%04x\n" $$((`wc -c < resident` - 2))
	@echo "Creating loader_acme$(SUFF).inc..."
	@#fetch all global labels for resident part
	@grep "bitfire_*" resident_labels.txt | sed "s/\s*;\s*.*//g" | tr -d "\t" | sed 's/\$$\([0-9a-f]\{3\}$$\)/\$$0\1/' | sed "s/\= / \= /" > loader_acme$(SUFF).inc
	@grep "link_*" resident_labels.txt | sed "s/\s*;\s*.*//g" | tr -d "\t" | sed 's/\$$\([0-9a-f]\{3\}$$\)/\$$0\1/' | sed "s/\= / \= /" >> loader_acme$(SUFF).inc
	@#fetch all config labels
	@grep "BITFIRE_*" resident_labels.txt | sed "s/\s*;\s*.*//g" | tr -d "\t" | sed 's/\$$\([0-9a-f]\{3\}$$\)/\$$0\1/' | sed "s/\= / \= /" >> loader_acme$(SUFF).inc
	@echo "Creating loader_c6510$(SUFF).inc..."
	@#convert to c6510 format (add const, wipe out underscores and add prefix with .)
	@echo "if def BITFIRE.ISINCLUDED == 0" > loader_c6510$(SUFF).inc
	@cat loader_acme$(SUFF).inc | sed "s/\(.*\)/const \\1/" >> loader_c6510$(SUFF).inc
	@echo "endif" >> loader_c6510$(SUFF).inc
	@echo ".importonce" > loader_kickass$(SUFF).inc
	@#cat loader_acme$(SUFF).inc | grep "bitfire" | sed "s/bitfire/\.var bitfire/" >> loader_kickass$(SUFF).inc
	@#cat loader_acme$(SUFF).inc | grep "BITFIRE" | sed "s/BITFIRE/\.var BITFIRE/" >> loader_kickass$(SUFF).inc
	@echo "Creating loader_kickass$(SUFF).inc..."
	@cat loader_acme$(SUFF).inc | sed "s/\(.*\)/\.var \\1/" >> loader_kickass$(SUFF).inc
	@#echo "}" >> loader_kickass$(SUFF).inc
	@cp resident resident$(SUFF)
	@cp loader_acme$(SUFF).inc loader_acme.inc

drivecode41.prg: drivecode.asm config.inc Makefile
	@$(ACME) $(ACMEOPT) -o $@ $<

drivecode : drivecode41.prg

#This is the parametrized target for building the installer, please note $(ACME_DEFS).
installer_p: installer.asm drivecode.asm request_disc.asm resident$(SUFF) detect.asm config.inc Makefile
	@echo "========================================================================================"
	@echo "Target: installer$(SUFF).prg"
	@echo "Building installer... (platform: $(PLATFORM); receiver: $(PLUS4_MODE); bincomp: $(PLUS4_BINCOMP))"
	@$(ACME) $(ACMEOPT) $(ACME_DEFS) -l installer_labels.txt -r installer.lst -o installer$(SUFF).prg installer.asm
	@echo "Creating drivecode_acme.inc..."
	@grep "drivecode_*" installer_labels.txt | sed "s/\s*;\s*.*//g" | tr -d "\t" | sed 's/\$$\([0-9a-f]\{3\}$$\)/\$$0\1/' | sed "s/\= / \= /" > drivecode_acme.inc
	@rm installer_labels.txt
	@rm resident_labels.txt

installer_c64.prg: Makefile resident.asm installer.asm config.inc
	@make -C . PLATFORM=64 PLUS4_MODE=0 PLUS4_BINCOMP=0 SUFF=_c64 installer_p
	@mv drivecode_acme.inc drivecode_acme_1541.inc

installer_plus4_1541sc.prg: Makefile resident.asm installer.asm config.inc
	@make -C . PLATFORM=16 PLUS4_MODE=1 PLUS4_BINCOMP=0 SUFF=_plus4_1541sc installer_p

installer_plus4_1541dc.prg: Makefile resident.asm installer.asm config.inc
	@make -C . PLATFORM=16 PLUS4_MODE=2 PLUS4_BINCOMP=0 SUFF=_plus4_1541dc installer_p
	
installer_plus4_1551.prg: Makefile resident.asm installer.asm config.inc
	@make -C . PLATFORM=16 PLUS4_MODE=8 PLUS4_BINCOMP=0 SUFF=_plus4_1551 installer_p
	@mv drivecode_acme.inc drivecode_acme_1551.inc

installer_plus4_1541sc_swap.prg: Makefile resident.asm installer.asm config.inc
	@echo "========================================================================================"
	@echo "Target: $(@)"
	@make -C . PLATFORM=16 PLUS4_MODE=2 PLUS4_BINCOMP=1 SUFF=_plus4_1541dc_bincomp resident_plus4_1541dc_bincomp
	@make -C . PLATFORM=16 PLUS4_MODE=1 PLUS4_BINCOMP=1 SUFF=_plus4_1541sc_bincomp resident_plus4_1541sc_bincomp
	@-diff -U 0 loader_acme_plus4_1541sc_bincomp.inc loader_acme_plus4_1541dc_bincomp.inc
	#@python cmp.py resident_p4_41sc resident_p4_41dc
	@make -C . PLATFORM=16 PLUS4_MODE=1 PLUS4_BINCOMP=2 SUFF=_plus4_1541sc_swap installer_p
	@grep -v BITFIRE_PLUS4_MODE loader_acme_plus4_1541sc_swap.inc > loader_acme_plus4_1541.inc 
	@grep -v BITFIRE_PLUS4_MODE loader_c6510_plus4_1541sc_swap.inc > loader_c6510_plus4_1541.inc 
	@grep -v BITFIRE_PLUS4_MODE loader_kickass_plus4_1541sc_swap.inc > loader_kickass_plus4_1541.inc
	
installer_plus4_1541dc_swap.prg: installer_plus4_1541sc_swap.prg
	@echo "========================================================================================"
	@echo "Target: $(@)"
	@make -C . PLATFORM=16 PLUS4_MODE=2 PLUS4_BINCOMP=2 SUFF=_plus4_1541dc_swap installer_p

installer_plus4_multi.prg: installer_plus4_1541sc_swap.prg installer_plus4_1541dc_swap.prg
	@echo "========================================================================================"
	@echo "Target: $(@)"
	@make -C . PLATFORM=16 PLUS4_MODE=8 PLUS4_BINCOMP=1 SUFF=_plus4_1551_bincomp resident_plus4_1551_bincomp

	@-diff loader_acme_plus4_1551_bincomp.inc loader_acme_plus4_1541.inc
	@$(ACME) $(ACMEOPT) -DMULTI_INST=1 -o installer_plus4_multi.prg installer.asm

	@cat loader_acme_plus4_1541.inc | sed "s/load_addr_lo/load_addr_lo_1541/g" > loader_acme_plus4_multi.inc 
	@grep load_addr_lo loader_acme_plus4_1551_bincomp.inc | sed "s/load_addr_lo/load_addr_lo_1551/g" >> loader_acme_plus4_multi.inc 

	@grep -v endif loader_c6510_plus4_1541.inc | sed "s/load_addr_lo/load_addr_lo_1541/g" > loader_c6510_plus4_multi.inc 
	@grep load_addr_lo loader_c6510_plus4_1551_bincomp.inc | sed "s/load_addr_lo/load_addr_lo_1551/g" >> loader_c6510_plus4_multi.inc 
	@echo endif >> loader_c6510_plus4_multi.inc 

	@cat loader_kickass_plus4_1541.inc | sed "s/load_addr_lo/load_addr_lo_1541/g" > loader_kickass_plus4_multi.inc 
	@grep load_addr_lo loader_kickass_plus4_1551_bincomp.inc | sed "s/load_addr_lo/load_addr_lo_1551/g" >> loader_kickass_plus4_multi.inc 

installers: installer_c64.prg installer_plus4_1541sc.prg installer_plus4_1541dc.prg installer_plus4_1551.prg installer_plus4_1541sc_swap.prg installer_plus4_1541dc_swap.prg installer_plus4_multi.prg
	@echo "All installers are compiled!"

drivecode_acme_1541.inc : installer_c64.prg

drivecode_acme_1551.inc : installer_plus4_1551.prg

loader_acme_c64.inc : installer_c64.prg

loader_acme_plus4_multi.inc : installer_plus4_multi.prg

loader_acme_plus4_1541.inc : installer_plus4_1541sc_swap.prg

loader_acme_plus4_1551.inc : installer_plus4_1551.prg

save_c64.prg : save.asm sector_routines.asm drivecode_acme_1541.inc loader_acme_c64.inc
	@$(ACME) $(ACMEOPT) -DBITFIRE_PLATFORM=64 -DBITFIRE_SAVE_ADDR=0x$(SAVE_C64_ADDR) -DBF_DRIVE=1541 -o $@ -l save_c64.txt save.asm
	@grep "bitfire_save*" save_c64.txt | sed "s/\s*;\s*.*//g" | tr -d "\t" | sed 's/\$$\([0-9a-f]\{3\}$$\)/\$$0\1/' | sed "s/_offs\= / \= BITFIRE_SAVE_ADDR + /" > save_acme.inc
	@cat save_acme.inc | sed "s/\(.*\)/const \\1/" > save_c6510.inc
	@echo ".importonce" > save_kickass.inc
	@cat save_acme.inc | sed "s/\(.*\)/\.var \\1/" >> save_kickass.inc
	@rm save_c64.txt

save_acme.inc : save_c64.prg

save_plus4_multi_1541.prg : save.asm sector_routines.asm drivecode_acme_1541.inc loader_acme_plus4_multi.inc
	@$(ACME) $(ACMEOPT) -DBITFIRE_PLATFORM=16 -DBITFIRE_SAVE_ADDR=0x$(SAVE_PLUS4_MULTI_ADDR) -DBITFIRE_PLUS4_MODE=11 -DBF_DRIVE=1541 -o $@ save.asm

save_plus4_multi_1551.prg : save.asm sector_routines.asm drivecode_acme_1551.inc loader_acme_plus4_multi.inc
	@$(ACME) $(ACMEOPT) -DBITFIRE_PLATFORM=16 -DBITFIRE_SAVE_ADDR=0x$(SAVE_PLUS4_MULTI_ADDR) -DBITFIRE_PLUS4_MODE=11 -DBF_DRIVE=1551 -o $@ save.asm

save_plus4_1551.prg : save.asm sector_routines.asm drivecode_acme_1551.inc loader_acme_plus4_1551.inc
	@$(ACME) $(ACMEOPT) -DBITFIRE_PLATFORM=16 -DBITFIRE_SAVE_ADDR=0x$(SAVE_PLUS4_1551_ADDR) -DBITFIRE_PLUS4_MODE=8 -DBF_DRIVE=1551 -o $@ save.asm

save_routines: save_c64.prg save_plus4_multi_1541.prg save_plus4_multi_1551.prg save_plus4_1551.prg
	@echo "All save routines are compiled!"

clean:
	@-rm bitnax d64write bitnax.exe d64write.exe installer*.prg save_*.inc save_*.prg loader_*.inc resident resident_c64 resident_plus4* resident_save*.prg resident*.txt *.lst drivecode*.inc

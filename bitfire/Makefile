ACME = acme
ACMEOPT = -f cbm
CFLAGS = -Os -Wall
LDLIBS =
CC = gcc

ACME_DEFS = -DBITFIRE_PLATFORM=$(PLATFORM) -DBITFIRE_PLUS4_MODE=$(PLUS4_MODE) -DBF_PLUS4_BINCOMP=$(PLUS4_BINCOMP)

all: bitnax d64write installer.prg

d64write: d64write.c *.h
	@echo "Buidling d64write..."
	@$(CC) $< -o $@ $(CFLAGS)

bitnax: lz.c
	@echo "Buidling bitnax..."
	@${CC} $< -o $@ ${CFLAGS}

installer.prg: installer.asm drivecode.asm request_disc.asm resident.asm detect.asm config.inc Makefile resident_minimal_zp.asm
	@echo -n "Buidling resident part ->"
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
	@echo "Buidling installer..."
	@$(ACME) $(ACMEOPT) -o installer.prg installer.asm
	@rm resident_labels.txt

#This is the parametrized target for building the resident, please note $(ACME_DEFS).
resident_p: resident.asm config.inc Makefile resident_minimal_zp.asm
	@echo "----------------------------------------------------------------------------------------"
	@echo -n "Buidling resident part (platform: $(PLATFORM); receiver: $(PLUS4_MODE); bincomp: $(PLUS4_BINCOMP)) ->"
	@$(ACME) $(ACMEOPT) $(ACME_DEFS) -o resident -l resident_labels.txt resident.asm
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

drivecode41.prg: drivecode.asm config.inc Makefile
	@$(ACME) $(ACMEOPT) -o $@ $<


drivecode : drivecode41.prg

#This is the parametrized target for building the installer, please note $(ACME_DEFS).
installer_p: installer.asm drivecode.asm request_disc.asm resident_p detect.asm config.inc Makefile resident_minimal_zp.asm
	@echo "========================================================================================"
	@echo "Buidling installer... (platform: $(PLATFORM); receiver: $(PLUS4_MODE); bincomp: $(PLUS4_BINCOMP))"
	@$(ACME) $(ACMEOPT) $(ACME_DEFS) -o installer_p installer.asm
	@rm resident_labels.txt


installer_c64.prg: Makefile resident.asm installer.asm config.inc
	@make -C . PLATFORM=64 PLUS4_MODE=0 PLUS4_BINCOMP=0 installer_p
	@mv installer_p $@
	@mv loader_acme.inc 		loader_acme_c64.inc 
	@mv loader_c6510.inc 	loader_c6510_c64.inc 
	@mv loader_kickass.inc 	loader_kickass_c64.inc 

installer_plus4_1541sc.prg: Makefile resident.asm installer.asm config.inc
	@make -C . PLATFORM=16 PLUS4_MODE=1 PLUS4_BINCOMP=0 installer_p
	@mv installer_p $@
	@mv loader_acme.inc 	loader_acme_plus4_1541sc.inc 
	@mv loader_c6510.inc 	loader_c6510_plus4_1541sc.inc 
	@mv loader_kickass.inc 	loader_kickass_plus4_1541sc.inc 

installer_plus4_1541dc.prg: Makefile resident.asm installer.asm config.inc
	@make -C . PLATFORM=16 PLUS4_MODE=2 PLUS4_BINCOMP=0 installer_p
	@mv installer_p $@
	@mv loader_acme.inc 	loader_acme_plus4_1541dc.inc 
	@mv loader_c6510.inc 	loader_c6510_plus4_1541dc.inc 
	@mv loader_kickass.inc 	loader_kickass_plus4_1541dc.inc 


installer_plus4_1551.prg: Makefile resident.asm installer.asm config.inc
	@make -C . PLATFORM=16 PLUS4_MODE=8 PLUS4_BINCOMP=0 installer_p
	@mv resident resident_1551
	@mv installer_p $@
	@mv loader_acme.inc 	loader_acme_plus4_1551.inc 
	@mv loader_c6510.inc 	loader_c6510_plus4_1551.inc 
	@mv loader_kickass.inc 	loader_kickass_plus4_1551.inc 

installer_plus4_1541sc_swap.prg: Makefile resident.asm installer.asm config.inc
	@echo "========================================================================================"
	@make -C . PLATFORM=16 PLUS4_MODE=2 PLUS4_BINCOMP=1 resident_p
	@mv resident resident_p4_41dc 
	@mv loader_acme.inc loader_acme_p4dc.inc
	@make -C . PLATFORM=16 PLUS4_MODE=1 PLUS4_BINCOMP=1 resident_p
	@mv resident resident_p4_41sc
	@mv loader_acme.inc loader_acme_p4sc.inc
	@diff -U 0 loader_acme_p4sc.inc loader_acme_p4dc.inc || true
	#@python cmp.py resident_p4_41sc resident_p4_41dc
	@make -C . PLATFORM=16 PLUS4_MODE=1 PLUS4_BINCOMP=2 installer_p
	@mv resident resident_1541sc
	@mv installer_p $@
	@grep -v BITFIRE_PLUS4_MODE loader_acme.inc > loader_acme_plus4_1541.inc 
	@grep -v BITFIRE_PLUS4_MODE loader_c6510.inc > loader_c6510_plus4_1541.inc 
	@grep -v BITFIRE_PLUS4_MODE loader_kickass.inc > loader_kickass_plus4_1541.inc
	@rm loader_acme.inc loader_c6510.inc loader_kickass.inc resident_p4_41dc

installer_plus4_1541dc_swap.prg: installer_plus4_1541sc_swap.prg
	@echo "========================================================================================"
	@make -C . PLATFORM=16 PLUS4_MODE=2 PLUS4_BINCOMP=2 installer_p
	@mv resident resident_1541dc
	@mv installer_p $@
	@rm loader_acme.inc loader_c6510.inc loader_kickass.inc resident_p4_41sc

installer_plus4_all.prg: installer_plus4_1541sc_swap.prg installer_plus4_1541dc_swap.prg
	@echo "========================================================================================"
	@make -C . PLATFORM=16 PLUS4_MODE=8 PLUS4_BINCOMP=1 resident_p
	@mv resident resident_1551

	@mv loader_acme.inc 	loader_acme_plus4_1551_bincomp.inc 
	@mv loader_c6510.inc 	loader_c6510_plus4_1551_bincomp.inc 
	@mv loader_kickass.inc 	loader_kickass_plus4_1551_bincomp.inc 

	diff loader_acme_plus4_1551_bincomp.inc loader_acme_plus4_1541.inc || true
	@$(ACME) $(ACMEOPT) -DMULTI_INST=1 -o installer_plus4_all.prg installer.asm

	@cat loader_acme_plus4_1541.inc | sed "s/plus4_sei/plus4_sei_1541/g" | sed "s/load_addr_lo/load_addr_lo_1541/g" > loader_acme_plus4_all.inc 
	@grep load_addr_lo loader_acme_plus4_1551_bincomp.inc | sed "s/load_addr_lo/load_addr_lo_1551/g" >> loader_acme_plus4_all.inc 

	@grep -v endif loader_c6510_plus4_1541.inc | sed "s/plus4_sei/plus4_sei_1541/g" | sed "s/load_addr_lo/load_addr_lo_1541/g" > loader_c6510_plus4_all.inc 
	@grep load_addr_lo loader_c6510_plus4_1551_bincomp.inc | sed "s/load_addr_lo/load_addr_lo_1551/g" >> loader_c6510_plus4_all.inc 
	@echo endif >> loader_c6510_plus4_all.inc 

	@cat loader_kickass_plus4_1541.inc | sed "s/plus4_sei/plus4_sei_1541/g" | sed "s/load_addr_lo/load_addr_lo_1541/g" > loader_kickass_plus4_all.inc 
	@grep load_addr_lo loader_kickass_plus4_1551_bincomp.inc | sed "s/load_addr_lo/load_addr_lo_1551/g" >> loader_kickass_plus4_all.inc 

installers: installer_c64.prg installer_plus4_1541sc.prg installer_plus4_1541dc.prg installer_plus4_1551.prg installer_plus4_1541sc_swap.prg installer_plus4_1541dc_swap.prg installer_plus4_all.prg
	@echo "All installers are compiled!"

clean:
	@-rm bitnax d64write installer*.prg loader_*.inc resident resident_15*
